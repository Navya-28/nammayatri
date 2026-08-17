-- TIP-MODULE-CONFIG v1: QAR-driven cadence for the rider "add tip" module.
-- Mirrors SharedLogic.TipModuleConfig.seedRulesV1 (keep in sync; unit test covers the rules).
-- Bands: qar absent -> {45,60,1}; <30% -> {15,30,3}; <60% -> {30,45,2}; else {60,0,1}.

DO $$
DECLARE
  v_merchant_id TEXT;
  v_city RECORD;
BEGIN
  SELECT m.id INTO v_merchant_id FROM atlas_app.merchant m WHERE m.short_id = 'NAMMA_YATRI' LIMIT 1;
  IF v_merchant_id IS NULL THEN
    RAISE NOTICE 'NAMMA_YATRI merchant not found, skipping TIP-MODULE-CONFIG seed';
    RETURN;
  END IF;

  INSERT INTO atlas_app.app_dynamic_logic_element (domain, merchant_id, version, logic, description, created_at, updated_at, "order") VALUES
    ('TIP-MODULE-CONFIG', v_merchant_id, 1,
     '{"cat":[{"var":""},{"qarPct":{"if":[{"==":[{"var":"qar"},null]},null,{"*":[100,{"var":"qar"}]}]}}]}',
     'qar (0..1) -> qarPct, null when absent', now(), now(), 0),
    ('TIP-MODULE-CONFIG', v_merchant_id, 1,
     '{"cat":[{"var":""},{"showAfterSec":{"if":[{"==":[{"var":"qarPct"},null]},45,{"if":[{"<":[{"var":"qarPct"},30]},15,{"if":[{"<":[{"var":"qarPct"},60]},30,60]}]}]}}]}',
     'showAfterSec by QAR band', now(), now(), 1),
    ('TIP-MODULE-CONFIG', v_merchant_id, 1,
     '{"cat":[{"var":""},{"repeatIntervalSec":{"if":[{"==":[{"var":"qarPct"},null]},60,{"if":[{"<":[{"var":"qarPct"},30]},30,{"if":[{"<":[{"var":"qarPct"},60]},45,0]}]}]}}]}',
     'repeatIntervalSec by QAR band', now(), now(), 2),
    ('TIP-MODULE-CONFIG', v_merchant_id, 1,
     '{"cat":[{"var":""},{"maxPrompts":{"if":[{"==":[{"var":"qarPct"},null]},1,{"if":[{"<":[{"var":"qarPct"},30]},3,{"if":[{"<":[{"var":"qarPct"},60]},2,1]}]}]}}]}',
     'maxPrompts by QAR band', now(), now(), 3)
  ON CONFLICT (domain, "order", version) DO UPDATE SET
    logic = EXCLUDED.logic, description = EXCLUDED.description, updated_at = now();

  FOR v_city IN SELECT id FROM atlas_app.merchant_operating_city WHERE merchant_id = v_merchant_id LOOP
    INSERT INTO atlas_app.app_dynamic_logic_rollout
      (domain, merchant_operating_city_id, percentage_rollout, time_bounds, version, version_description, merchant_id, created_at, updated_at)
    VALUES ('TIP-MODULE-CONFIG', v_city.id, 100, 'Unbounded', 1, 'QAR bands v1', v_merchant_id, now(), now())
    ON CONFLICT DO NOTHING;

    -- Per-city fallback used when rules yield nothing (e.g. rollout 0%).
    UPDATE atlas_app.rider_config
    SET tip_module_config = '{"showAfterSec":45,"repeatIntervalSec":60,"maxPrompts":1}'::json,
        updated_at = now()
    WHERE merchant_operating_city_id = v_city.id AND tip_module_config IS NULL;
  END LOOP;

  RAISE NOTICE 'TIP-MODULE-CONFIG v1 seeded and rolled out at 100%% for NAMMA_YATRI cities';
END $$;
