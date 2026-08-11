-- One-time backfill: copy access_token/base_url/api_version out of
-- merchant_service_config's Meta_CloudApi row into the newly-added columns
-- on atlas_app.meta_config (renamed from meta_webhook_config in
-- ddl-migrations/rider-app/1561-meta-config-rename-and-access-token.sql).
--
-- access_token transfers as-is: same Passetto envelope format
-- (version|keyId|ciphertext) app_secret/verify_token already use on this
-- table, so no decrypt/re-encrypt round trip.
UPDATE atlas_app.meta_config mc
SET access_token = msc.config_json ->> 'access_token',
    base_url = msc.config_json ->> 'base_url',
    api_version = msc.config_json ->> 'api_version'
FROM atlas_app.merchant_service_config msc
WHERE msc.service_name = 'Meta_CloudApi'
  AND msc.merchant_id = mc.merchant_id
  AND msc.merchant_operating_city_id = mc.merchant_operating_city_id;
