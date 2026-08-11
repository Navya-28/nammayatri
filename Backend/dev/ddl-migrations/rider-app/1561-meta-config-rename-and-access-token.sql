-- Renames meta_webhook_config -> meta_config: this table now holds BOTH the
-- inbound webhook-verification secrets (app_secret/verify_token) AND the
-- outbound send credentials (access_token/base_url/api_version, moved here
-- from merchant_service_config's Meta_CloudApi row) — "webhook config" only
-- described the inbound half. The Haskell type/module name stays
-- MetaWebhookConfig (see spec/Storage/MetaWebhookConfig.yaml); only the
-- physical table name changes here.
--
-- New columns are nullable: existing rows get NULL until the one-time
-- backfill (dev/feature-migrations, DML doesn't belong in ddl-migrations)
-- runs. Tighten to NOT NULL in a follow-up migration once every enabled row
-- has a value and the old merchant_service_config row is retired.
ALTER TABLE IF EXISTS atlas_app.meta_webhook_config RENAME TO meta_config;

ALTER TABLE atlas_app.meta_config ADD COLUMN IF NOT EXISTS access_token text;
ALTER TABLE atlas_app.meta_config ADD COLUMN IF NOT EXISTS base_url text;
ALTER TABLE atlas_app.meta_config ADD COLUMN IF NOT EXISTS api_version text;
