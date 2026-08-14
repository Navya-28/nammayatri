module SharedLogic.ScheduledBooking.WindowValidation
  ( validateScheduledBookingWindow,
  )
where

import Kernel.Prelude
import Kernel.Utils.Common
import Tools.Error (ScheduledBookingError (..))

-- Defence-in-depth BPP guard: reject a scheduled booking whose lead time
-- (pickup - now) falls outside the configured [min, max] advance window.
-- Each bound is skipped when its config is Nothing, so both-Nothing keeps
-- existing behaviour. Window bounds are primarily enforced by the BAP.
validateScheduledBookingWindow ::
  Maybe Seconds ->
  Maybe Seconds ->
  UTCTime ->
  UTCTime ->
  Either ScheduledBookingError ()
validateScheduledBookingWindow mbMinBookingWindow mbMaxBookingWindow now pickupTime
  | maybe False (leadTime <) mbMinBookingWindow = Left ScheduledBookingWindowTooSoon
  | maybe False (leadTime >) mbMaxBookingWindow = Left ScheduledBookingWindowTooFarInFuture
  | otherwise = Right ()
  where
    leadTime = nominalDiffTimeToSeconds (diffUTCTime pickupTime now)
