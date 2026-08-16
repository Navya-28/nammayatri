-- ─── SWS-4 · Overlap / multi-hold ─────────────────────────────────────────────
UPDATE atlas_driver_offer_bpp.transporter_config
SET max_scheduled_holds_per_driver = 3,
    scheduled_ride_avg_speed_kmph  = 16,
    schedule_ride_buffer_time      = 1800
WHERE merchant_operating_city_id = :msil_city_id;