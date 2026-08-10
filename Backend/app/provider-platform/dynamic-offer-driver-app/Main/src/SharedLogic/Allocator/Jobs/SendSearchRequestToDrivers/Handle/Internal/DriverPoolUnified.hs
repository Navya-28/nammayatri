module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPoolUnified where

import qualified Control.Monad.Catch as C
import Control.Monad.Extra (partitionM)
import qualified Dashboard.Common as DC
import Data.Aeson
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as DL
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Domain.Action.UI.Driver (acceptDynamicOfferDriverRequest)
import Domain.Types.Common
import Domain.Types.DriverGoHomeRequest as DDGR
import Domain.Types.DriverPoolConfig
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import Domain.Types.GoHomeConfig (GoHomeConfig)
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Driver)
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SearchRequestForDriver (DriverSearchRequestStatus (Inactive))
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleServiceTier as DVST
import EulerHS.Prelude hiding (id)
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude (NominalDiffTime)
import Kernel.Storage.Clickhouse.Config
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics.Types (CoreMetrics, DeploymentVersion)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.DatastoreLatencyCalculator
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Queries.GateInfo
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import qualified Lib.Types.SpecialLocation as SL
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.AirportEntryFee as AirportEntryFee
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool as SDP
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.SendSearchRequestToDrivers (buildSearchRequestForDriver)
import qualified SharedLogic.Beckn.Common as DTS
import qualified SharedLogic.CallInternalMLPricing as ML
import SharedLogic.DriverPool
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.Finance.Prepaid as SFPrepaid
import qualified SharedLogic.Finance.Wallet as SFWallet
import SharedLogic.Ride (offerQuoteLockKeyWithCoolDown)
import Storage.Beam.SpecialZone ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDrQt
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import Storage.Queries.Person.GetNearestDrivers (isDriverModeEligibleHelper)
import qualified Storage.Queries.RiderDriverCorrelation as QFavDrivers
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error (DriverInformationError (DriverInfoNotFound))
import Tools.Maps as Maps
import TransactionLogs.Types (KeyConfig, TokenConfig)

getNextDriverPoolBatch ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    LT.HasLocationService m r,
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    Redis.HedisLTSFlowEnv r,
    Redis.HedisFlow m r,
    HasField "secondaryLTSHedisEnv" r (Maybe Redis.HedisEnv),
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r,
    HasField "enableLtsPoolDataForPooling" r Bool,
    BeamFlow m r,
    CoreMetrics m,
    MonadReader r m,
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HashMap.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HashMap.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasField "serviceClickhouseCfg" r ClickhouseCfg,
    HasField "serviceClickhouseEnv" r ClickhouseEnv,
    HasField "driverQuoteExpirationSeconds" r NominalDiffTime,
    HasField "version" r DeploymentVersion,
    HasFlowEnv m r '["version" ::: DeploymentVersion],
    HasHttpClientOptions r c,
    EventStreamFlow m r,
    HasPrettyLogger m r,
    ServiceFlow m r,
    HasField "quoteRespondCoolDown" r Int,
    HasField "driverUnlockDelay" r Seconds
  ) =>
  DriverPoolConfig ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  [TripQuoteDetail] ->
  Maybe DMPM.PaymentMethodInfo ->
  GoHomeConfig ->
  m DriverPoolWithActualDistResultWithFlags
getNextDriverPoolBatch driverPoolConfig searchReq searchTry tripQuoteDetails paymentMethodInfo goHomeConfig = withLogTag "getNextDriverPoolBatch" do
  logDebug $ "Doing Special Driver Pooling for seachReq:- " <> show searchReq
  batchNum <- SDP.getPoolBatchNum searchTry.id
  SDP.incrementBatchNum searchTry.id
  cityServiceTiers <- CQVST.findAllByMerchantOpCityIdInRideFlow searchReq.merchantOperatingCityId (searchReq.area >>= SL.pickupSpecialZoneIdFromArea)
  merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound searchReq.providerId.getId)
  withTimeAPI "driverPooling" "prepareDriverPoolBatch" $ prepareDriverPoolBatch cityServiceTiers merchant driverPoolConfig searchReq searchTry tripQuoteDetails batchNum goHomeConfig paymentMethodInfo

assignTagsToDrivers :: [Id Driver] -> DriverPoolTags -> [DriverPoolWithActualDistResult] -> [DriverPoolWithActualDistResult]
assignTagsToDrivers driverIds driverTag =
  map
    ( \dp ->
        if dp.driverPoolResult.driverId `elem` driverIds
          then
            dp
              { driverPoolResult =
                  (driverPoolResult dp :: DriverPoolResult)
                    { driverTags =
                        insertInObject
                          (((driverTags :: DriverPoolResult -> Value) . driverPoolResult) dp)
                          [driverTag]
                    }
              }
          else dp
    )

prepareDriverPoolBatch ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    LT.HasLocationService m r,
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    Redis.HedisLTSFlowEnv r,
    Redis.HedisFlow m r,
    HasField "secondaryLTSHedisEnv" r (Maybe Redis.HedisEnv),
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r,
    HasField "enableLtsPoolDataForPooling" r Bool,
    BeamFlow m r,
    CoreMetrics m,
    MonadReader r m,
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HashMap.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HashMap.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasField "serviceClickhouseCfg" r ClickhouseCfg,
    HasField "serviceClickhouseEnv" r ClickhouseEnv,
    HasField "driverQuoteExpirationSeconds" r NominalDiffTime,
    HasField "version" r DeploymentVersion,
    HasFlowEnv m r '["version" ::: DeploymentVersion],
    HasHttpClientOptions r c,
    EventStreamFlow m r,
    HasPrettyLogger m r,
    ServiceFlow m r,
    HasField "quoteRespondCoolDown" r Int,
    HasField "driverUnlockDelay" r Seconds
  ) =>
  [DVST.VehicleServiceTier] ->
  DM.Merchant ->
  DriverPoolConfig ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  [TripQuoteDetail] ->
  PoolBatchNum ->
  GoHomeConfig ->
  Maybe DMPM.PaymentMethodInfo ->
  m DriverPoolWithActualDistResultWithFlags
prepareDriverPoolBatch cityServiceTiers merchant driverPoolCfg searchReq searchTry tripQuoteDetails startingbatchNum goHomeConfig paymentMethodInfo = withLogTag ("startingbatchNum- (" <> show startingbatchNum <> ")" <> " for txnId:- " <> show searchReq.transactionId) $ do
  isValueAddNP <- CQVAN.isValueAddNP searchReq.bapId
  previousBatchesDrivers <- getPreviousBatchesDrivers Nothing
  previousBatchesDriversOnRide <- getPreviousBatchesDrivers (Just True)
  let merchantOpCityId = searchReq.merchantOperatingCityId
  logDebug $ "PreviousBatchesDrivers-" <> show previousBatchesDrivers
  -- Fetched once, threaded to both prepareDriverPoolBatch' and attemptPriorityDirectAssign --
  -- avoids each re-fetching the same config per batch on this hot allocator loop.
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigDoesNotExist merchantOpCityId.getId)
  SDP.PrepareDriverPoolBatchEntity {..} <- withTimeAPI "driverPooling" "prepareDriverPoolBatch'" $ prepareDriverPoolBatch' previousBatchesDrivers startingbatchNum merchantOpCityId searchReq.transactionId isValueAddNP transporterConfig
  let finalPool = currentDriverPoolBatch <> currentDriverPoolBatchOnRide
  SDP.incrementDriverRequestCount finalPool searchTry.id
  -- Priority direct-assign only targets not-on-ride candidates; on-ride drivers always broadcast normally.
  notOnRideAfterPriority <- attemptPriorityDirectAssign merchant searchReq searchTry tripQuoteDetails cityServiceTiers driverPoolCfg startingbatchNum transporterConfig currentDriverPoolBatch
  let poolToBroadcast = notOnRideAfterPriority <> currentDriverPoolBatchOnRide
  pure $ buildDriverPoolWithActualDistResultWithFlags poolToBroadcast poolType nextScheduleTime (previousBatchesDrivers <> previousBatchesDriversOnRide)
  where
    buildDriverPoolWithActualDistResultWithFlags finalPool poolType nextScheduleTime prevBatchDrivers =
      DriverPoolWithActualDistResultWithFlags
        { driverPoolWithActualDistResult = finalPool,
          poolType = poolType,
          prevBatchDrivers = prevBatchDrivers,
          nextScheduleTime = nextScheduleTime
        }
    getPreviousBatchesDrivers ::
      ( EncFlow m r,
        EsqDBReplicaFlow m r,
        EsqDBFlow m r,
        CacheFlow m r,
        LT.HasLocationService m r,
        HasKafkaProducer r,
        ClickhouseFlow m r
      ) =>
      Maybe Bool ->
      m [Id Driver]
    getPreviousBatchesDrivers mbOnRide = do
      batches <- SDP.previouslyAttemptedDrivers searchTry.id mbOnRide
      return $ fst <$> batches

    prepareDriverPoolBatch' previousBatchesDrivers batchNum merchantOpCityId txnId isValueAddNP batchTransporterConfig = withLogTag ("BatchNum - " <> show batchNum <> " and txnId:- " <> show txnId) $ do
      -- Renamed to avoid shadowing: nested where-helpers below (calcDriverPool,
      -- calculateNormalBatch, etc.) already have their own local `transporterConfig`, and a
      -- same-named param (unlike a do-bind) would be visible to them.
      let transporterConfig = batchTransporterConfig
      airportEntryFee <-
        if fromMaybe False transporterConfig.airportEntryFeeCheckAtStartRide
          then pure Nothing
          else AirportEntryFee.requiredEntryFeeForBooking (fromMaybe False transporterConfig.airportEntryFeeEnabled) searchReq.pickupGateId
      isAirportRequest <- AirportEntryFee.isAirportPickupArea searchReq.area
      blockListedDriversForSearch <- Redis.withCrossAppRedis $ Redis.getList (mkBlockListedDriversKey searchReq.id)
      blockListedDriversForRider <- maybe (pure []) (Redis.withCrossAppRedis . Redis.getList . mkBlockListedDriversForRiderKey) searchReq.riderId
      let blockListedDrivers = blockListedDriversForSearch <> blockListedDriversForRider
      -- Blocklisted drivers are excluded at LTS-level inside calculateDriverPoolWithActualDist;
      -- previously-attempted drivers are sorted to the tail of LTS candidates (chunking only
      -- pulls them in if fresher drivers run out — replaces the old fillBatch backfill).
      (allDriversNotOnRide', allOnRideDriverPoolResults) <- withTimeAPI "driverPooling" "calcDriverPool" $ calcDriverPool NormalPool transporterConfig blockListedDrivers previousBatchesDrivers airportEntryFee isAirportRequest
      favDrivers <- maybe (pure []) (`QFavDrivers.findFavDriversForRider` True) searchReq.riderId
      let newFilteredDriversWithFavourites = assignTagsToDrivers (favDrivers <&> (.driverId)) FavouriteDriver allDriversNotOnRide'
      (driverPoolNotOnRide, driverPoolOnRide) <- do
        case batchNum of
          -1 -> do
            gateTaggedDrivers <- assignDriverGateTags searchReq newFilteredDriversWithFavourites
            goHomeTaggedDrivers <- assignDriverGoHomeTags gateTaggedDrivers searchReq searchTry tripQuoteDetails driverPoolCfg merchant goHomeConfig merchantOpCityId isValueAddNP transporterConfig paymentMethodInfo
            logDebug $ "GoHomeDriverPool and GateTaggedPool-" <> show goHomeTaggedDrivers
            withTimeAPI "driverPooling" "calculateNormalBatchGoHome" $ calculateNormalBatch merchantOpCityId transporterConfig (bookAnyFilters transporterConfig goHomeTaggedDrivers) txnId allOnRideDriverPoolResults airportEntryFee isAirportRequest
          _ -> do
            allNearbyNonGoHomeDrivers <- withTimeAPI "driverPooling" "filterM getDriverGoHomeRequestInfo" $ filterM (\dpr -> (CQDGR.getDriverGoHomeRequestInfo dpr.driverPoolResult.driverId merchantOpCityId (Just goHomeConfig)) <&> (/= Just DDGR.ACTIVE) . (.status)) newFilteredDriversWithFavourites
            logDebug $ "Calculating Normal Batch for the pool " <> show allNearbyNonGoHomeDrivers
            withTimeAPI "driverPooling" "calculateNormalBatch" $ calculateNormalBatch merchantOpCityId transporterConfig (bookAnyFilters transporterConfig allNearbyNonGoHomeDrivers) txnId allOnRideDriverPoolResults airportEntryFee isAirportRequest
      cacheBatch driverPoolNotOnRide Nothing
      cacheBatch driverPoolOnRide (Just True)
      let (poolNotOnRide, poolOnRide) =
            ( addDistanceSplitConfigBasedDelaysForDriversWithinBatch driverPoolNotOnRide,
              addDistanceSplitConfigBasedDelaysForOnRideDriversWithinBatch driverPoolOnRide
            )
      (poolWithSpecialZoneInfoNotOnRide, poolWithSpecialZoneInfoOnRide) <-
        if isJust searchReq.specialLocationTag
          then (,) <$> addSpecialZonePickupInfo poolNotOnRide <*> addSpecialZonePickupInfo poolOnRide
          else pure (poolNotOnRide, poolOnRide)
      pure $ SDP.PrepareDriverPoolBatchEntity poolWithSpecialZoneInfoNotOnRide NormalPool Nothing poolWithSpecialZoneInfoOnRide
      where
        addSpecialZonePickupInfo pool = do
          (driversFromGate, restDrivers) <- splitDriverFromGateAndRest pool
          pure $ restDrivers <> addSpecialZoneInfo searchReq.driverDefaultExtraFee driversFromGate

        splitDriverFromGateAndRest pool =
          case searchReq.pickupZoneGateId of
            Just pickupZoneGateId ->
              partitionM
                ( \dd -> do
                    mbDriverGate <- findGateInfoIfDriverInsideGatePickupZone (LatLong dd.driverPoolResult.lat dd.driverPoolResult.lon)
                    pure $ case mbDriverGate of
                      Just driverGate -> driverGate.id.getId == pickupZoneGateId
                      Nothing -> False
                )
                pool
            Nothing -> pure ([], pool)

        calcDriverPool poolType transporterConfig excludeDriverIds prevAttemptedDriverIds airportEntryFee isAirportRequest = do
          now <- getCurrentTime
          let serviceTiers = tripQuoteDetails <&> (.vehicleServiceTier)
              merchantId = searchReq.providerId
              pickupLoc = searchReq.fromLocation
              pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
              dropLocation = searchReq.toLocation <&> (\loc -> LatLong loc.lat loc.lon)
              routeDistance = searchReq.estimatedDistance
              currentSearchInfo = DTS.CurrentSearchInfo {..}
              govtCharges = listToMaybe tripQuoteDetails >>= (.govtCharges)
              tollCharges_ = listToMaybe tripQuoteDetails >>= (.tollCharges)
              parkingCharge = listToMaybe tripQuoteDetails >>= (.driverParkingCharge)
              currentRideTripCategoryValidForForwardBatching = driverPoolCfg.currentRideTripCategoryValidForForwardBatching
              driverPoolReq =
                CalculateDriverPoolReq
                  { poolStage = DriverSelection,
                    pickup = pickupLatLong,
                    merchantOperatingCityId = merchantOpCityId,
                    isRental = isRentalTrip searchTry.tripCategory,
                    isInterCity = isInterCityTrip searchTry.tripCategory,
                    onlinePayment = merchant.onlinePayment,
                    rideFare = Just searchTry.baseFare,
                    tollCharges = tollCharges_,
                    paymentInstrument = fmap (.paymentInstrument) paymentMethodInfo,
                    paymentMode = searchReq.paymentMode,
                    excludeDriverIds = excludeDriverIds,
                    prevAttemptedDriverIds = prevAttemptedDriverIds,
                    ..
                  }
          calculateDriverPoolWithActualDist driverPoolReq poolType currentSearchInfo batchNum

        calculateNormalBatch mOCityId transporterConfig onlyNewNormalDrivers txnId' onRidePoolResults airportEntryFee isAirportRequest = do
          logDebug $ "calculateNormalBatch txnId " <> show txnId'
          (normalBatchNotOnRide, _, _) <- withTimeAPI "driverPooling" "getDriverPoolNotOnRide" $ getDriverPoolNotOnRide mOCityId transporterConfig onlyNewNormalDrivers
          logDebug $ "NormalBatchNotOnRide-" <> show normalBatchNotOnRide <> " and txnId " <> show txnId'
          normalBatchOnRide <- getDriverPoolOnRide mOCityId transporterConfig NormalPool onRidePoolResults airportEntryFee isAirportRequest
          pure (normalBatchNotOnRide, normalBatchOnRide)

        getDriverPoolNotOnRide mOCityId transporterConfig onlyNewNormalDrivers = do
          (_, normalDriverPoolBatch) <- withTimeAPI "driverPooling" "mkDriverPoolBatch" $ mkDriverPoolBatch mOCityId onlyNewNormalDrivers transporterConfig batchSize False
          pure (normalDriverPoolBatch, [], Nothing)

        filtersForNormalBatch mOCityId transporterConfig normalDriverPool = do
          allNearbyNonGoHomeDrivers <- filterM (\dpr -> (CQDGR.getDriverGoHomeRequestInfo dpr.driverPoolResult.driverId mOCityId (Just goHomeConfig)) <&> (/= Just DDGR.ACTIVE) . (.status)) normalDriverPool
          pure $ bookAnyFilters transporterConfig allNearbyNonGoHomeDrivers

        getDriverPoolOnRide mOCityId transporterConfig poolType allDriverPoolResults airportEntryFee isAirportRequest = do
          if poolType == NormalPool && driverPoolCfg.enableForwardBatching && searchTry.isAdvancedBookingEnabled
            then do
              previousDriverOnRide <- getPreviousBatchesDrivers (Just True)
              allNearbyDriversCurrentlyOnRide <- calcDriverCurrentlyOnRidePool poolType transporterConfig batchNum allDriverPoolResults airportEntryFee isAirportRequest
              logDebug $ "NormalDriverPoolBatchOnRideCurrentlyOnRide-" <> show allNearbyDriversCurrentlyOnRide
              onlyNewNormalDriversOnRide <- filtersForNormalBatch mOCityId transporterConfig allNearbyDriversCurrentlyOnRide
              (_, normalDriverPoolBatchOnRide) <- withTimeAPI "driverPooling" "mkDriverPoolBatchOnRide" $ mkDriverPoolBatch mOCityId onlyNewNormalDriversOnRide transporterConfig batchSizeOnRide True
              validDriversFromPreviousBatch <-
                filterM
                  ( \dpr -> do
                      isHasValidRequests <- SDP.checkRequestCount searchTry.id (SDP.isBookAny $ tripQuoteDetails <&> (.vehicleServiceTier)) dpr.driverPoolResult.driverId dpr.driverPoolResult.serviceTier dpr.driverPoolResult.serviceTierDowngradeLevel driverPoolCfg
                      let isPreviousBatchDriver = dpr.driverPoolResult.driverId `elem` previousDriverOnRide
                      return $ isHasValidRequests && isPreviousBatchDriver
                  )
                  allNearbyDriversCurrentlyOnRide
              logDebug $ "NormalDriverPoolBatchOnRide-" <> show normalDriverPoolBatchOnRide
              logDebug $ "ValidDriversFromPreviousBatchOnRide-" <> show validDriversFromPreviousBatch
              let finalBatchOnRide = take batchSizeOnRide $ normalDriverPoolBatchOnRide <> validDriversFromPreviousBatch
              pure finalBatchOnRide
            else pure []

        bookAnyFilters transporterConfig allNearbyDrivers = do
          if SDP.isBookAny (tripQuoteDetails <&> (.vehicleServiceTier))
            then do selectMinDowngrade transporterConfig.bookAnyVehicleDowngradeLevel allNearbyDrivers
            else do allNearbyDrivers

        -- This function takes a list of DriverPoolWithActualDistResult and returns a list with unique driverId entries with the minimum serviceTierDowngradeLevel.
        selectMinDowngrade :: Int -> [DriverPoolWithActualDistResult] -> [DriverPoolWithActualDistResult]
        selectMinDowngrade config results = Map.elems $ foldr insertOrUpdate Map.empty filtered
          where
            insertOrUpdate :: DriverPoolWithActualDistResult -> Map.Map Text DriverPoolWithActualDistResult -> Map.Map Text DriverPoolWithActualDistResult
            insertOrUpdate result currentMap =
              let driver = result.driverPoolResult
                  key = driver.driverId.getId
               in Map.insertWith minByDowngradeLevel key result currentMap

            minByDowngradeLevel :: DriverPoolWithActualDistResult -> DriverPoolWithActualDistResult -> DriverPoolWithActualDistResult
            minByDowngradeLevel new old =
              if new.driverPoolResult.serviceTierDowngradeLevel < old.driverPoolResult.serviceTierDowngradeLevel
                then new
                else old

            filtered = filter (\d -> d.driverPoolResult.serviceTierDowngradeLevel >= config) results

        mkDriverPoolBatch mOCityId onlyNewDrivers transporterConfig batchSize' isOnRidePool = withTimeAPI "driverPooling" "makeTaggedDriverPool" $ SDP.makeTaggedDriverPool mOCityId transporterConfig.timeDiffFromUtc searchReq onlyNewDrivers batchSize' isOnRidePool searchReq.customerNammaTags searchReq.poolingLogicVersion batchNum driverPoolCfg searchTry.id

        addDistanceSplitConfigBasedDelaysForDriversWithinBatch =
          addDelaysWithPrioritySplit driverPoolCfg.distanceBasedBatchSplit

        addDistanceSplitConfigBasedDelaysForOnRideDriversWithinBatch =
          addDelaysWithPrioritySplit driverPoolCfg.onRideBatchSplitConfig

        addDelaysWithPrioritySplit ::
          ( HasField "batchSplitSize" split Int,
            HasField "batchSplitDelay" split Seconds
          ) =>
          [split] ->
          [DriverPoolWithActualDistResult] ->
          [DriverPoolWithActualDistResult]
        addDelaysWithPrioritySplit splits pool =
          if not enablePriorityTagSplit' || null priorityTagNames || null priorityDrivers
            then applyDelays splits pool
            else firstSplitWithDelay <> restWithDelay
          where
            enablePriorityTagSplit' = fromMaybe False driverPoolCfg.enablePriorityTagSplit
            priorityTagNames = extractPriorityDriverTags searchReq.searchTags
            (priorityDrivers, nonPriorityDrivers) = DL.partition (hasAnyPriorityTag priorityTagNames) pool
            mbFirstSplit = listToMaybe splits
            firstSplitDelay = maybe (Seconds 0) (.batchSplitDelay) mbFirstSplit
            firstSplitSize = maybe 0 (.batchSplitSize) mbFirstSplit
            backfillCount = max 0 (firstSplitSize - length priorityDrivers)
            restSplits = drop 1 splits
            firstSplitWithDelay = map (\d -> d {keepHiddenForSeconds = firstSplitDelay}) priorityDrivers
            lastIdx = length restSplits - 1
            restWithDelay =
              fst $
                foldl'
                  ( \(finalBatch, (restBatch, idx)) splitConfig ->
                      let extraSize = if idx == lastIdx then backfillCount else 0
                          (splitToAddDelay, newRestBatch) = splitAt (splitConfig.batchSplitSize + extraSize) restBatch
                          splitWithDelay = map (\d -> d {keepHiddenForSeconds = splitConfig.batchSplitDelay}) splitToAddDelay
                       in (finalBatch <> splitWithDelay, (newRestBatch, idx + 1))
                  )
                  ([], (nonPriorityDrivers, 0 :: Int))
                  restSplits

            applyDelays splits' pool' =
              fst $
                foldl'
                  ( \(finalBatch, restBatch) splitConfig ->
                      let (splitToAddDelay, newRestBatch) = splitAt splitConfig.batchSplitSize restBatch
                          splitWithDelay = map (\d -> d {keepHiddenForSeconds = splitConfig.batchSplitDelay}) splitToAddDelay
                       in (finalBatch <> splitWithDelay, newRestBatch)
                  )
                  ([], pool')
                  splits'

        calcDriverCurrentlyOnRidePool poolType transporterConfig _batchNum allDriverPoolResults airportEntryFee isAirportRequest = do
          let merchantId = searchReq.providerId
          now <- getCurrentTime
          if transporterConfig.includeDriverCurrentlyOnRide && driverPoolCfg.enableForwardBatching
            then do
              let onRideDrivers = filterOnRideDriversFromPool allDriverPoolResults
              let serviceTiers = tripQuoteDetails <&> (.vehicleServiceTier)
              let pickupLoc = searchReq.fromLocation
              let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
              let dropLocation = searchReq.toLocation <&> (\loc -> LatLong loc.lat loc.lon)
                  routeDistance = searchReq.estimatedDistance
              let currentSearchInfo = DTS.CurrentSearchInfo {..}
              let govtCharges = listToMaybe tripQuoteDetails >>= (.govtCharges)
                  tollCharges_ = listToMaybe tripQuoteDetails >>= (.tollCharges)
                  parkingCharge = listToMaybe tripQuoteDetails >>= (.driverParkingCharge)
              let currentRideTripCategoryValidForForwardBatching = driverPoolCfg.currentRideTripCategoryValidForForwardBatching
              let driverPoolReq =
                    CalculateDriverPoolReq
                      { poolStage = DriverSelection,
                        pickup = pickupLatLong,
                        merchantOperatingCityId = merchantOpCityId,
                        isRental = isRentalTrip searchTry.tripCategory,
                        isInterCity = isInterCityTrip searchTry.tripCategory,
                        onlinePayment = merchant.onlinePayment,
                        rideFare = Just searchTry.baseFare,
                        tollCharges = tollCharges_,
                        paymentInstrument = fmap (.paymentInstrument) paymentMethodInfo,
                        paymentMode = searchReq.paymentMode,
                        excludeDriverIds = [],
                        prevAttemptedDriverIds = [],
                        ..
                      }
              calculateDriverCurrentlyOnRideWithActualDist driverPoolReq onRideDrivers poolType currentSearchInfo
            else pure []

        cacheBatch batch consideOnRideDrivers = do
          logDebug $ "Caching batch-" <> show batch
          batches <- SDP.previouslyAttemptedDrivers searchTry.id consideOnRideDrivers
          let minimalBatch = (\dp -> (dp.driverPoolResult.driverId, dp.driverPoolResult.serviceTier)) <$> batch
          Redis.withCrossAppRedis $ Redis.setExp (SDP.previouslyAttemptedDriversKey searchTry.id consideOnRideDrivers) (batches <> minimalBatch) (60 * 30)

        batchSize = getBatchSize driverPoolCfg.dynamicBatchSize batchNum driverPoolCfg.driverBatchSize
        batchSizeOnRide = driverPoolCfg.batchSizeOnRide

assignDriverGateTags ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    LT.HasLocationService m r
  ) =>
  DSR.SearchRequest ->
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
assignDriverGateTags searchReq pool = do
  case searchReq.pickupZoneGateId of
    Just pickupZoneGateId -> do
      (onGateDrivers', outSideDrivers) <-
        partitionM
          ( \dd -> do
              mbDriverGate <- findGateInfoIfDriverInsideGatePickupZone (LatLong dd.driverPoolResult.lat dd.driverPoolResult.lon)
              pure $ case mbDriverGate of
                Just driverGate -> driverGate.id.getId == pickupZoneGateId
                Nothing -> False
          )
          pool
      let onGateDrivers'' =
            map
              ( \(dp :: DriverPoolWithActualDistResult) ->
                  let dpResult = dp.driverPoolResult
                      updatedTags = insertInObject dpResult.driverTags [SpecialZoneQueueDriver]
                      dprWithTags = (dpResult :: DriverPoolResult) {driverTags = updatedTags}
                   in dp {driverPoolResult = dprWithTags}
              )
              onGateDrivers'
          onGateDrivers = addSpecialZoneInfo searchReq.driverDefaultExtraFee onGateDrivers''
      return $ onGateDrivers <> outSideDrivers
    Nothing -> return pool

addSpecialZoneInfo :: Maybe HighPrecMoney -> [DriverPoolWithActualDistResult] -> [DriverPoolWithActualDistResult]
addSpecialZoneInfo driverDefaultExtraFee = map (\driverWithDistance -> driverWithDistance {pickupZone = True, specialZoneExtraTip = driverDefaultExtraFee})

assignDriverGoHomeTags ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    LT.HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    Redis.HedisLTSFlowEnv r
  ) =>
  [DriverPoolWithActualDistResult] ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  [TripQuoteDetail] ->
  DriverPoolConfig ->
  DM.Merchant ->
  GoHomeConfig ->
  Id MerchantOperatingCity ->
  Bool ->
  DTC.TransporterConfig ->
  Maybe DMPM.PaymentMethodInfo ->
  m [DriverPoolWithActualDistResult]
assignDriverGoHomeTags pool searchReq searchTry tripQuoteDetails driverPoolCfg merchant goHomeConfig merchantOpCityId isValueAddNP transporterConfig paymentMethodInfo = do
  (goHomeDriversInQueue, goHomeDriversNotToDestination) <-
    case searchReq.toLocation of
      Just toLoc | isGoHomeAvailable searchTry.tripCategory -> do
        let dropLocation = searchReq.toLocation <&> (\loc -> LatLong loc.lat loc.lon)
            routeDistance = searchReq.estimatedDistance
        let currentSearchInfo = DTS.CurrentSearchInfo {..}
        let goHomeReq =
              CalculateGoHomeDriverPoolReq
                { poolStage = DriverSelection,
                  driverPoolCfg = driverPoolCfg,
                  goHomeCfg = goHomeConfig,
                  serviceTiers = tripQuoteDetails <&> (.vehicleServiceTier),
                  fromLocation = searchReq.fromLocation,
                  toLocation = toLoc, -- last or all ?
                  merchantId = searchReq.providerId,
                  isRental = isRentalTrip searchTry.tripCategory,
                  isInterCity = isInterCityTrip searchTry.tripCategory,
                  onlinePayment = merchant.onlinePayment,
                  configsInExperimentVersions = searchReq.configInExperimentVersions,
                  rideFare = Just searchTry.baseFare,
                  govtCharges = listToMaybe tripQuoteDetails >>= (.govtCharges),
                  tollCharges = listToMaybe tripQuoteDetails >>= (.tollCharges),
                  parkingCharge = listToMaybe tripQuoteDetails >>= (.driverParkingCharge),
                  paymentInstrument = fmap (.paymentInstrument) paymentMethodInfo,
                  paymentMode = searchReq.paymentMode,
                  ..
                }
        filterOutGoHomeDriversAccordingToHomeLocation (map (convertDriverPoolWithActualDistResultToNearestGoHomeDriversResult False True) pool) goHomeReq merchantOpCityId
      _ -> pure ([], [])
  let goHomeDriversToDestionation = map (\dp -> dp.driverPoolResult.driverId) goHomeDriversInQueue
      goHomePool' = filter (\dp -> dp.driverPoolResult.driverId `notElem` goHomeDriversToDestionation) pool <> assignGoHomeTags goHomeDriversToDestionation GoHomeDriverToDestination False goHomeDriversInQueue
  return $ assignGoHomeTags goHomeDriversNotToDestination GoHomeDriverNotToDestination True goHomePool'
  where
    assignGoHomeTags goHomeDrivers driverTag checkNeeded =
      map
        ( \dp ->
            if not checkNeeded || dp.driverPoolResult.driverId `elem` goHomeDrivers
              then
                dp
                  { driverPoolResult =
                      (driverPoolResult dp :: DriverPoolResult)
                        { driverTags =
                            insertInObject
                              (((driverTags :: DriverPoolResult -> Value) . driverPoolResult) dp)
                              [driverTag]
                        }
                  }
              else dp
        )

insertInObject :: Value -> [DriverPoolTags] -> Value
insertInObject obj tags =
  case obj of
    Object keymap -> Object $ DL.foldl' (\acc key -> AKM.insert ((AK.fromString . show) key) (toJSON True) acc) keymap tags
    _ -> Object $ DL.foldl' (\acc key -> AKM.insert ((AK.fromString . show) key) (toJSON True) acc) AKM.empty tags

hasAnyPriorityTag :: [Text] -> DriverPoolWithActualDistResult -> Bool
hasAnyPriorityTag tagNames dp = case dp.driverPoolResult.driverTags of
  Object keymap -> any (\name -> AKM.member (AK.fromText name) keymap) tagNames
  _ -> False

priorityDriverTagPrefix :: Text
priorityDriverTagPrefix = "priorityDriverTag#"

extractPriorityDriverTags :: Maybe [LYT.TagNameValue] -> [Text]
extractPriorityDriverTags = maybe [] (mapMaybe extractTag)
  where
    extractTag (LYT.TagNameValue raw) = case T.stripPrefix priorityDriverTagPrefix raw of
      Just name | not (T.null name) -> Just name
      _ -> Nothing

-- | For each batch, before broadcast, try to directly assign one of this batch's
-- InstantAssign#<tier>-tagged drivers (tiers with instantAcceptanceConfig.enabled=true),
-- nearest-first, without ever notifying them — only the resulting "ride assigned" push (via the
-- rider's existing autoAssignEnabledV2 auto-confirm chain) tells them anything happened. On
-- success, returns [] (nothing broadcasts, the ride's already taken); on failure, returns the
-- batch unchanged for normal broadcast. Only targets not-on-ride candidates.
attemptPriorityDirectAssign ::
  forall m r c.
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    LT.HasLocationService m r,
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    Redis.HedisLTSFlowEnv r,
    Redis.HedisFlow m r,
    HasField "secondaryLTSHedisEnv" r (Maybe Redis.HedisEnv),
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r,
    HasField "enableLtsPoolDataForPooling" r Bool,
    BeamFlow m r,
    CoreMetrics m,
    MonadReader r m,
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HashMap.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HashMap.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasField "serviceClickhouseCfg" r ClickhouseCfg,
    HasField "serviceClickhouseEnv" r ClickhouseEnv,
    HasField "driverQuoteExpirationSeconds" r NominalDiffTime,
    HasField "version" r DeploymentVersion,
    HasFlowEnv m r '["version" ::: DeploymentVersion],
    HasHttpClientOptions r c,
    EventStreamFlow m r,
    HasPrettyLogger m r,
    ServiceFlow m r,
    HasField "quoteRespondCoolDown" r Int,
    HasField "driverUnlockDelay" r Seconds,
    C.MonadCatch m
  ) =>
  DM.Merchant ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  [TripQuoteDetail] ->
  [DVST.VehicleServiceTier] ->
  DriverPoolConfig ->
  PoolBatchNum ->
  DTC.TransporterConfig ->
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
attemptPriorityDirectAssign merchant searchReq searchTry tripQuoteDetails cityServiceTiers driverPoolCfg batchNum batchTransporterConfig batch = do
  -- Renamed to avoid shadowing tryAssign's own local `transporterConfig` param below (a
  -- same-named param, unlike a do-bind, would be visible to that where-clause).
  let transporterConfig = batchTransporterConfig
  let instantAcceptanceConfigForTier tier =
        DL.find (\vst -> vst.serviceTierType == tier) cityServiceTiers >>= (.instantAcceptanceConfig)
      isInstantAssignEnabledForTier tier =
        maybe False (.enabled) (instantAcceptanceConfigForTier tier)
      -- driverTags is keyed by bare category with the tier name as the value, e.g.
      -- {"InstantAssign": "COMFY"} -- not a composite "InstantAssign<tier>" key.
      -- "Cohort" is a separate category, scoped to rider-facing estimate visibility
      -- (Gate 1/2 + LTS's per-tag GEO bucket) -- it plays no role in this assignment-time check.
      hasPriorityTag tierName dp = case dp.driverPoolResult.driverTags of
        Object keymap -> case AKM.lookup (AK.fromString "InstantAssign") keymap of
          Just (String v) -> v == tierName
          _ -> False
        _ -> False
      -- Checked directly against the typed field carried on DriverPoolResult (populated in
      -- GetNearestDrivers.hs's mkResultHelper straight from DriverPoolData, no JSON tag
      -- involved) rather than via a driverTags marker -- avoids the encode/decode mismatch
      -- class of bug entirely, since a typed field access can't silently fail to match.
      isPriorityCandidate dp =
        isInstantAssignEnabledForTier dp.driverPoolResult.serviceTier
          && hasPriorityTag (show dp.driverPoolResult.serviceTier) dp
          && dp.driverPoolResult.serviceTier `elem` dp.driverPoolResult.selectedInstantAcceptTiers
      priorityCandidates = DL.filter isPriorityCandidate batch
      sortedPriority = DL.sortOn (.actualDistanceToPickup) priorityCandidates
  if null sortedPriority
    then pure batch
    else do
      let tripQuoteDetailsHashMap = HashMap.fromList $ (\tqd -> (tqd.vehicleServiceTier, tqd)) <$> tripQuoteDetails
      now <- getCurrentTime
      let validTill = fromIntegral driverPoolCfg.singleBatchProcessTime `addUTCTime` now
      quoteRespondCoolDown <- asks (.quoteRespondCoolDown)
      driverUnlockDelay <- asks (.driverUnlockDelay)
      assigned <- tryAssign instantAcceptanceConfigForTier transporterConfig tripQuoteDetailsHashMap validTill quoteRespondCoolDown driverUnlockDelay sortedPriority
      -- On success return [], not the untagged remainder -- the ride's already assigned, so
      -- broadcasting it would still push a "ride available" notice for a ride already taken.
      pure $ if assigned then [] else batch
  where
    tryAssign ::
      (ServiceTierType -> Maybe DC.InstantAcceptanceConfig) ->
      DTC.TransporterConfig ->
      HashMap.HashMap ServiceTierType TripQuoteDetail ->
      UTCTime ->
      Int ->
      Seconds ->
      [DriverPoolWithActualDistResult] ->
      m Bool
    tryAssign _ _ _ _ _ _ [] = pure False
    tryAssign instantAcceptanceConfigForTier transporterConfig tripQuoteDetailsHashMap validTill quoteRespondCoolDown driverUnlockDelay (dp : rest) = do
      let driverId = cast dp.driverPoolResult.driverId
          unlockThisDriver = Redis.unlockRedis (offerQuoteLockKeyWithCoolDown driverId)
      locked <- Redis.tryLockRedis (offerQuoteLockKeyWithCoolDown driverId) quoteRespondCoolDown
      if not locked
        then tryAssign instantAcceptanceConfigForTier transporterConfig tripQuoteDetailsHashMap validTill quoteRespondCoolDown driverUnlockDelay rest
        else do
          result <- C.try $ do
            driverInfo <- QDI.findById driverId >>= fromMaybeM DriverInfoNotFound
            -- Same double-booking guard respondQuote has (thereAreActiveQuotes, Driver.hs),
            -- inlined since that's a private where-closure there. Needed because onRide only
            -- flips True once initializeRide runs asynchronously -- there's a window after the
            -- DriverQuote is created where another batch could re-select this same driver.
            hasActiveQuoteElsewhere <- not . null <$> QDrQt.findActiveQuotesByDriverId driverId driverUnlockDelay
            -- Re-check live eligibility: `dp`'s active/mode/tiers came from a Redis snapshot
            -- taken when the batch was built, up to one wave's cadence ago. respondQuote gets
            -- this for free from the driver's own accept-tap; this silent path has no such
            -- signal, so it must re-read it.
            mbVehicle <- QVehicle.findById driverId
            let stillHasTierSelected = maybe False ((dp.driverPoolResult.serviceTier `elem`) . (.selectedServiceTiers)) mbVehicle
                stillHasAutoAcceptTierSelected = maybe False ((dp.driverPoolResult.serviceTier `elem`) . fromMaybe [] . (.selectedInstantAcceptTiers)) mbVehicle
            walletOk <- case instantAcceptanceConfigForTier dp.driverPoolResult.serviceTier >>= (.minWalletBalance) of
              Nothing -> pure True
              Just minBalance -> do
                mbBalance <- SFWallet.getWalletBalanceByOwner SFPrepaid.counterpartyDriver driverId.getId
                pure $ maybe False (>= minBalance) mbBalance
            let isStillLive =
                  not driverInfo.blocked
                    && driverInfo.enabled
                    && driverInfo.subscribed
                    && isDriverModeEligibleHelper driverInfo.mode driverInfo.active
                    && stillHasTierSelected
                    && stillHasAutoAcceptTierSelected
                    && walletOk
            if driverInfo.onRide || hasActiveQuoteElsewhere || not isStillLive
              then pure False
              else do
                sReqFD <- buildSearchRequestForDriver searchTry searchReq tripQuoteDetailsHashMap batchNum validTill transporterConfig searchReq.riderId Map.empty dp
                QSRD.createMany [sReqFD]
                driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
                driverStats <- QDriverStats.findById driverId >>= fromMaybeM DriverInfoNotFound
                void $ acceptDynamicOfferDriverRequest Nothing merchant.id searchReq.merchantOperatingCityId merchant searchTry searchReq driver sReqFD Nothing Nothing Nothing Nothing Nothing Nothing driverStats transporterConfig
                now <- getCurrentTime
                QSRD.updateDriverResponse (Just Accept) Inactive Nothing (Just now) (Just now) sReqFD.id
                pure True
          case result of
            Right True -> pure True -- success: lock intentionally stays held, released later by initializeRide, same as the real accept flow
            Right False -> unlockThisDriver >> tryAssign instantAcceptanceConfigForTier transporterConfig tripQuoteDetailsHashMap validTill quoteRespondCoolDown driverUnlockDelay rest
            Left (_ :: SomeException) -> unlockThisDriver >> tryAssign instantAcceptanceConfigForTier transporterConfig tripQuoteDetailsHashMap validTill quoteRespondCoolDown driverUnlockDelay rest
