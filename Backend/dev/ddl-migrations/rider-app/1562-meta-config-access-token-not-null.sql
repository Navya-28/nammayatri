-- Tightens access_token/base_url/api_version to NOT NULL now that the
-- one-time backfill from merchant_service_config's Meta_CloudApi row
-- (dev/feature-migrations/0047-meta-config-access-token-backfill.sql) has
-- run and every enabled row has a value. See
-- 1561-meta-config-rename-and-access-token.sql for where these columns
-- were added (nullable, at the time).
ALTER TABLE atlas_app.meta_config ALTER COLUMN access_token SET NOT NULL;
ALTER TABLE atlas_app.meta_config ALTER COLUMN base_url SET NOT NULL;
ALTER TABLE atlas_app.meta_config ALTER COLUMN api_version SET NOT NULL;
