-- IF NOT EXISTS guards throughout: the generator's own read-only rollup
-- (dev/migrations-read-only/rider-app/meta_webhook_config.sql) already
-- creates this table's columns via the schema-init step that runs before
-- the numbered migration sequence in this dev harness; this migration must
-- be a no-op when that's already happened. The unique index isn't covered
-- by the rollup, so it's the one statement that actually does new work here.
CREATE TABLE IF NOT EXISTS atlas_app.meta_webhook_config ();

ALTER TABLE atlas_app.meta_webhook_config ADD COLUMN IF NOT EXISTS id character varying(36) NOT NULL;
ALTER TABLE atlas_app.meta_webhook_config ADD COLUMN IF NOT EXISTS phone_number_id text NOT NULL;
ALTER TABLE atlas_app.meta_webhook_config ADD COLUMN IF NOT EXISTS app_secret text NOT NULL;
ALTER TABLE atlas_app.meta_webhook_config ADD COLUMN IF NOT EXISTS verify_token text NOT NULL;
ALTER TABLE atlas_app.meta_webhook_config ADD COLUMN IF NOT EXISTS merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.meta_webhook_config ADD COLUMN IF NOT EXISTS merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.meta_webhook_config ADD COLUMN IF NOT EXISTS bot_config jsonb NOT NULL;
ALTER TABLE atlas_app.meta_webhook_config ADD COLUMN IF NOT EXISTS enabled boolean NOT NULL DEFAULT true;
ALTER TABLE atlas_app.meta_webhook_config ADD COLUMN IF NOT EXISTS created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.meta_webhook_config ADD COLUMN IF NOT EXISTS updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;

-- Primary key not added here — the rollup's `ADD PRIMARY KEY ( id)` already
-- ran; a second attempt errors ("multiple primary keys for table are not
-- allowed"), and there's no IF NOT EXISTS form for that in Postgres.

-- Unique, not just indexed: a phoneNumberId maps to exactly one config row —
-- that's the whole point of this table, enforced at the DB level.
CREATE UNIQUE INDEX IF NOT EXISTS idx_meta_webhook_config_phone_number_id ON atlas_app.meta_webhook_config (phone_number_id);
