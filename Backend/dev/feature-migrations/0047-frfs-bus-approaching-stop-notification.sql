-- FRFS bus "approaching stop" push notifications, fired by gps-processor once per stop per
-- distance-threshold crossing (relaxed ~1km, nearing ~300m), via
-- POST /internal/frfs/trip/{tripId}/stop/{stopCode}/notifyApproaching (see FRFSInternal +
-- Domain.Action.UI.FRFSTicketService.notifyBusApproachingStopForTrip).
--
-- Tier-gated per city on rider_config.bus_approaching_notification_tiers (a dedicated whitelist,
-- separate from BUS_TRIP_STARTED's bus_tracking_notification_tiers).
--
-- Sound: reuses the existing TRIP_UPDATED notification_sounds_config row for the city (no new
-- sound config needed).
--
-- Scope: ONLY merchant_operating_city_id = de93a406-aa99-4db9-8691-2baa1258d4d0
-- (ANNA_APP / Chennai — premium bus, not the shuttle city used by 0039/0042/0045).
--
-- Idempotent: safe to re-run.

------------------------------------------------------------------------------------------------------
-- Feature flag / tier gate (ONLY this city)
------------------------------------------------------------------------------------------------------
UPDATE atlas_app.rider_config
SET
    bus_approaching_notification_tiers = '{PREMIUM}',
    updated_at = CURRENT_TIMESTAMP
WHERE merchant_operating_city_id = 'de93a406-aa99-4db9-8691-2baa1258d4d0';

------------------------------------------------------------------------------------------------------
-- Push: bus ~1km from stop
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type,
    key,
    merchant_id,
    merchant_operating_city_id,
    title,
    body,
    language,
    should_trigger,
    created_at,
    updated_at
)
SELECT
    'TRIGGER_FCM',
    'BUS_APPROACHING_1KM',
    moc.merchant_id,
    moc.id,
    'Your bus is on its way!',
    'Bus {#routeDisplay#} on route {#routeName#} is about 1 km from {#stopName#}.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE moc.id = 'de93a406-aa99-4db9-8691-2baa1258d4d0'
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'BUS_APPROACHING_1KM'
      AND mpn.merchant_operating_city_id = moc.id
);

------------------------------------------------------------------------------------------------------
-- Push: bus ~300m from stop
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type,
    key,
    merchant_id,
    merchant_operating_city_id,
    title,
    body,
    language,
    should_trigger,
    created_at,
    updated_at
)
SELECT
    'TRIGGER_FCM',
    'BUS_APPROACHING_300M',
    moc.merchant_id,
    moc.id,
    'Your bus is almost here!',
    'Bus {#routeDisplay#} on route {#routeName#} is about 300 m from {#stopName#}.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE moc.id = 'de93a406-aa99-4db9-8691-2baa1258d4d0'
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'BUS_APPROACHING_300M'
      AND mpn.merchant_operating_city_id = moc.id
);
