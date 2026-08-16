{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Multi-hold scheduled-ride overlap checks. The driver's committed set is DERIVED from
-- ride+booking (UPCOMING/NEW/INPROGRESS) — nothing new is persisted. Holds are pairwise
-- non-overlapping by construction (this check runs on every accept), so a candidate only
-- needs a travel+buffer gap against its two neighbours.
module SharedLogic.ScheduledBooking.OverlapCheck where

import Data.List (sortOn)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Domain.Types.TransporterConfig (TransporterConfig)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Maps as TMaps

-- | One committed ride as a time interval anchored at its pickup/drop positions.
data CommittedInterval = CommittedInterval
  { intervalStart :: UTCTime,
    intervalEnd :: UTCTime,
    intervalPickup :: LatLong,
    intervalDrop :: LatLong
  }
  deriving (Generic, Show)

-- | The ride being considered (a new scheduled search or a board booking).
data ScheduledCandidate = ScheduledCandidate
  { candidateStart :: UTCTime,
    candidateEnd :: UTCTime,
    candidatePickup :: LatLong,
    candidateDrop :: Maybe LatLong
  }
  deriving (Generic, Show)

committedRideStatuses :: [DRide.RideStatus]
committedRideStatuses = [DRide.UPCOMING, DRide.NEW, DRide.INPROGRESS]

-- | The driver's committed set, sorted by scheduled pickup time. Derived on demand — always fresh.
getDriverCommittedRides :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m [(DRide.Ride, DRB.Booking)]
getDriverCommittedRides driverId = do
  rides <- QRide.findAllByDriverIdAndStatuses driverId committedRideStatuses
  pairs <- forM rides $ \ride -> fmap (\booking -> (ride, booking)) <$> QBooking.findById ride.bookingId
  pure $ sortOn ((.startTime) . snd) (catMaybes pairs)

-- | Holds = UPCOMING rides only (NEW/INPROGRESS are the trip underway, not a held slot).
countActiveHolds :: [(DRide.Ride, DRB.Booking)] -> Int
countActiveHolds = length . filter (\(ride, _) -> ride.status == DRide.UPCOMING)

mkCommittedIntervals :: UTCTime -> Maybe (Id DRB.Booking) -> [(DRide.Ride, DRB.Booking)] -> [CommittedInterval]
mkCommittedIntervals now mbExcludeBookingId = sortOn (.intervalStart) . mapMaybe mk
  where
    mk (ride, booking)
      | maybe False (booking.id ==) mbExcludeBookingId = Nothing
      | otherwise =
        let duration = maybe 0 secondsToNominalDiffTime booking.estimatedDuration
            pickupPos = LatLong booking.fromLocation.lat booking.fromLocation.lon
            dropPos = maybe pickupPos (\loc -> LatLong loc.lat loc.lon) booking.toLocation
         in case ride.status of
              DRide.UPCOMING -> Just $ CommittedInterval booking.startTime (addUTCTime duration booking.startTime) pickupPos dropPos
              DRide.NEW -> Just $ CommittedInterval booking.startTime (addUTCTime duration booking.startTime) pickupPos dropPos
              -- trip underway occupies [now, planned end]; it can only ever be a predecessor, so its drop is what matters
              DRide.INPROGRESS ->
                let plannedEnd = addUTCTime duration (fromMaybe booking.startTime ride.tripStartTime)
                 in Just $ CommittedInterval now (max now plannedEnd) pickupPos dropPos
              _ -> Nothing

mkCandidateFromBooking :: DRB.Booking -> ScheduledCandidate
mkCandidateFromBooking booking =
  ScheduledCandidate
    { candidateStart = booking.startTime,
      candidateEnd = addUTCTime (maybe 0 secondsToNominalDiffTime booking.estimatedDuration) booking.startTime,
      candidatePickup = LatLong booking.fromLocation.lat booking.fromLocation.lon,
      candidateDrop = (\loc -> LatLong loc.lat loc.lon) <$> booking.toLocation
    }

-- | Insert-interval feasibility: the candidate must clear (end + deadhead + buffer) against its
-- immediate predecessor and successor only. Deadhead = real routed distance / per-city avg speed.
-- An unknowable leg fails CLOSED; a distance-API error propagates (same as the existing
-- scheduledRideFilter) — concrete-Flow callers wrap with withTryCatch to fail closed.
isCandidateFeasible ::
  (MonadFlow m, ServiceFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  TransporterConfig ->
  ScheduledCandidate ->
  [CommittedInterval] ->
  m Bool
isCandidateFeasible merchantId merchantOpCityId transporterConfig candidate committed = do
  let mbPredecessor = listToMaybe . reverse $ filter (\ci -> ci.intervalStart <= candidate.candidateStart) committed
      mbSuccessor = listToMaybe $ filter (\ci -> ci.intervalStart > candidate.candidateStart) committed
  predecessorOk <- case mbPredecessor of
    Nothing -> pure True
    Just predecessor -> legFeasible predecessor.intervalDrop predecessor.intervalEnd candidate.candidatePickup candidate.candidateStart
  if not predecessorOk
    then pure False
    else case mbSuccessor of
      Nothing -> pure True
      Just successor -> case candidate.candidateDrop of
        Nothing -> pure False -- unknown drop: cannot prove the gap to the next hold
        Just candidateDrop -> legFeasible candidateDrop candidate.candidateEnd successor.intervalPickup successor.intervalStart
  where
    buffer = transporterConfig.scheduleRideBufferTime
    avgSpeedKmph = fromMaybe 25.0 transporterConfig.scheduledRideAvgSpeedKmph
    legFeasible fromPos fromEnd toPos toStart =
      if addUTCTime buffer fromEnd > toStart
        then pure False -- even a zero-length deadhead cannot make it
        else do
          resp <-
            TMaps.getDistanceForScheduledRides merchantId merchantOpCityId Nothing $
              TMaps.GetDistanceReq
                { origin = fromPos,
                  destination = toPos,
                  travelMode = Just TMaps.CAR,
                  sourceDestinationMapping = Nothing,
                  distanceUnit = Meter
                }
          let deadheadKm = (fromIntegral resp.distance.getMeters :: Double) / 1000
              deadheadSeconds = realToFrac (deadheadKm / avgSpeedKmph * 3600) :: NominalDiffTime
          pure $ addUTCTime (deadheadSeconds + buffer) fromEnd <= toStart

-- | Earliest remaining UPCOMING hold (excluding the one being released) — relies on
-- getDriverCommittedRides' pickup-time ordering.
earliestRemainingScheduledHold :: Maybe (Id DRB.Booking) -> [(DRide.Ride, DRB.Booking)] -> Maybe (UTCTime, LatLong)
earliestRemainingScheduledHold mbExcludeBookingId pairs =
  listToMaybe
    [ (booking.startTime, LatLong booking.fromLocation.lat booking.fromLocation.lon)
      | (ride, booking) <- pairs,
        ride.status == DRide.UPCOMING,
        maybe True (booking.id /=) mbExcludeBookingId
    ]

-- | What latest_scheduled_booking/pickup should become after releasing a hold.
-- Single-slot (cap <= 1) short-circuits to Nothing — today's unconditional clear, no fetch.
nextScheduledHoldAfterRelease :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => TransporterConfig -> Id DP.Person -> Id DRB.Booking -> m (Maybe (UTCTime, LatLong))
nextScheduledHoldAfterRelease transporterConfig driverId releasedBookingId
  | transporterConfig.maxScheduledHoldsPerDriver <= 1 = pure Nothing
  | otherwise = earliestRemainingScheduledHold (Just releasedBookingId) <$> getDriverCommittedRides driverId
