# QAR-driven Tip Module Cadence Config for the Rider Search Screen

**Date:** 2026-08-16
**Service:** rider-app (BAP) + one enum constructor in `lib/yudhishthira`
**Scope:** Backend only. Rider frontend consumes the new fields; the UI work lives in a separate repo.

## Problem

While a search is in progress the rider app can show an "add tip" module. Adding a tip
cancels the current search and re-fires `/select2` with a `customerExtraFee`. Today the
app has no signal for **when to first show** that module, **how often to repeat** it, or
**how many times** — and product wants that cadence to depend on how likely drivers are
to accept in the current city/area right now (**QAR — quote acceptance rate**), so that
low acceptance nudges early and often while healthy acceptance nudges little or not at all.

Nothing exists for this today: `RiderConfig` has no tip fields, `/select2` returns no
timer, and no per-estimate cadence is exposed. What *does* exist:

- The BPP computes QAR at search time and already ships it in `on_search` as the `QAR`
  INFO tag; rider-app persists it as `estimate.qar` (`spec/Storage/estimate.yaml:138`)
  but does not return it in the estimate API.
- The BPP's `DYNAMIC_PRICING_UNIFIED` JSON-logic domain already turns QAR into
  `smartTipSuggestion` / `smartTipReason` / `tipOptions`, all exposed on
  `EstimateAPIEntity`.
- `/select2` is already the "cancel previous estimate + re-select with tip" primitive
  (`API/UI/Select.hs:112-129`, per-person Redis lock).
- Rider-app already hosts small JSON-logic decision domains modelled on
  `SharedLogic/PickupETA.hs` (`PICKUP_ETA_CALCULATION`): input record → versioned rules
  → output record, with per-city percentage rollout and a ClickHouse debug log.

## Decisions taken during design

| Question | Decision | Why |
|---|---|---|
| What the UI needs | **Time-based**: `showAfterSec`, `repeatIntervalSec`, `maxPrompts` | Simple contract; independent of allocator batch internals |
| Where the QAR → cadence mapping runs | **Rider-side JSON-logic domain** on persisted `estimate.qar` | QAR already crosses to the BAP; no BPP/Beckn change; mid-search re-evaluation explicitly not wanted, so search-time QAR is sufficient |
| Why JSON logic and not static bands | Sibling decisions (smart tip, ETA, dues) are rule domains; thresholds are unknown and will be experimented on; conditions will grow beyond QAR (tier, distance, time window); rollout / A/B / debug log come free | Static bands would be the odd one out and would need a deploy per new condition |
| Fallback when rules yield nothing | **Per-city default on `RiderConfig`**, else `null` (UI hides the module) | Nudging still works before rules are rolled out; ops control per city |
| Extras in scope | Expose `estimate.qar` on the API entity; add the missing `validTill` check to `/select2` | Cheap, adjacent, both surfaced by the survey |

Out of scope: mid-search re-evaluation of QAR, any BPP or Beckn-spec change, the
`mbActualQARCity = Nothing` quirk in BPP `FarePolicy.hs`, making the `1..100000` tip
bound configurable, multimodal (`Lib/JourneyLeg/Taxi.hs` hardcodes no tip).

## Design

### 1. Data type and API contract

```haskell
data TipModuleConfig = TipModuleConfig
  { showAfterSec      :: Int   -- first prompt after N seconds of searching (from select2)
  , repeatIntervalSec :: Int   -- re-prompt cadence; 0 = never repeat
  , maxPrompts        :: Int   -- hard cap per search
  } deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
```

`EstimateAPIEntity` (`Domain/Action/UI/Estimate.hs`) gains two fields:

- `tipModuleConfig :: Maybe TipModuleConfig` — rule result if any, else the city's
  `RiderConfig.tipModuleConfig`, else `null`.
- `qar :: Maybe Double` — the persisted `estimate.qar`, informational.

UI behaviour (for the frontend team, not enforced by backend): on `select2` start a
timer; show the module at `showAfterSec`; repeat every `repeatIntervalSec` while prompts
shown `< maxPrompts`; if `tipModuleConfig` is `null`, never show. `repeatIntervalSec = 0`
means show once at most.

### 2. New rule domain `TIP_MODULE_CONFIG`

`lib/yudhishthira/src/Lib/Yudhishthira/Types.hs` — add constructor `TIP_MODULE_CONFIG`
to `LogicDomain`, wired exactly like `PICKUP_ETA_CALCULATION`: `Enumerable` list,
`Show` as `"TIP-MODULE-CONFIG"`, `Read` arm. This is the only touch outside rider-app;
every app that pattern-matches on `LogicDomain` must still compile under `-Werror`.

New rider-app module `src/SharedLogic/TipModuleConfig.hs` (modelled on `PickupETA.hs`):

```haskell
data TipModuleConfigInput = TipModuleConfigInput
  { qar                   :: Maybe Double          -- estimate.qar; null when BPP sent none
  , serviceTier           :: ServiceTierType
  , estimatedDistanceInKm :: Maybe Double
  , isValueAddNP          :: Bool
  } deriving (Generic, Show, ToJSON, FromJSON)   -- + Default instance for the dashboard

getTipModuleConfigFromModel
  :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, ClickhouseFlow m r)
  => Id SearchRequest -> Id MerchantOperatingCity -> Seconds -> TipModuleConfigInput
  -> m (Maybe TipModuleConfig)
```

Behaviour:

1. `localTime` from the city's UTC offset (needed for time-bound rollouts such as `PeakHours`).
2. `getAppDynamicLogic cityId LYT.TIP_MODULE_CONFIG localTime Nothing (Just toss)` where
   `toss` is derived deterministically from the `SearchRequest` id (same idea as
   `poolingLogicVersionToss` on the BPP), so every `/results` poll within one search
   evaluates the same version.
3. If no logics → `Nothing`.
4. `runLogicsWithDebugLog LYDL.Rider cityId LYT.TIP_MODULE_CONFIG (Just searchReqId) logics input`,
   wrapped in `withTryCatch`.
5. `A.fromJSON resp.result :: Result TipModuleConfig`; `Success` → `Just`, `Error` →
   `logWarning`, `Nothing`. Never throws to the caller.

`TipModuleConfig` (the output type) is defined in `Domain/Types/Extra/RiderConfig.hs` (the
generated `Domain.Types.RiderConfig` cannot import `SharedLogic.*`) and re-exported from this
module for the API entity. **Post-review:** the logic program is fetched once per
`/results` request (`fetchTipModuleConfigLogics`) and only the run + decode happens per estimate.

### 3. Per-city default on `RiderConfig`

`spec/Storage/RiderConfig.yaml`: `tipModuleConfig: Maybe TipModuleConfig`, imported by
full module path, stored as a JSON column via `toTType`/`fromTType`
(same pattern as `boostSearchPreSelectionServiceTierConfig`, `RiderConfig.yaml:221,333`).
Regenerate; the DSL emits `dev/migrations-read-only/rider-app/rider_config.sql`.

No config-pilot work is needed for this field: it is already covered by the
`RIDER_CONFIG RiderConfig` patch domain, so ops can A/B the *default* through
config pilot without further code.

### 4. Wiring into the estimate response

`mkEstimateAPIEntity` (`Domain/Action/UI/Estimate.hs:107`) is the single builder; callers
(`Domain/Action/UI/Quote.hs:500` and any other) already run in a monad with cache/DB
access. Change:

- Callers fetch the city's `RiderConfig` once per request (not per estimate) and pass
  `riderConfig.tipModuleConfig` in (new argument, or a small record if the parameter list
  is already long — decided at implementation time to keep the signature readable).
- Inside the builder, per estimate:
  ```haskell
  let input = TipModuleConfigInput
        { qar, serviceTier = vehicleServiceTierType
        , estimatedDistanceInKm = metersToKm <$> (estimatedDistance <&> (.value))
        , isValueAddNP = valueAddNPRes }
  fromRules <- getTipModuleConfigFromModel requestId cityId tz input
  let tipModuleConfig = fromRules <|> defaultTipModuleConfig
  ```
  `cityId` is `estimate.merchantOperatingCityId` falling back to the search request's
  city, as done elsewhere in the builder's callers.
- `qar` is passed straight through.

Estimates are ~5–10 per search and yudhishthira reads elements/rollouts through cached
queries, so per-estimate evaluation has the same cost profile as
`PICKUP_ETA_CALCULATION`, which already runs on every driver-location poll.

### 5. Dashboard registration

`Domain/Action/Dashboard/NammaTag.hs`: add a `LYTU.TIP_MODULE_CONFIG` arm to the verify
switch (~L371: `createLogicData def` + `verifyAndUpdateDynamicLogic … (Proxy :: Proxy TipModuleConfig)`)
and to the domain-schema switch (~L595: `def :: TipModuleConfigInput` +
`toInlinedSchemaValue (Proxy @TipModuleConfigInput)`). This is what lets ops author,
verify and roll out rules from the dashboard with a schema and sample input.

### 6. `select2` expiry guard

The same `estimate.validTill` check that v1 `select` performs (`Domain/Action/UI/Select.hs`)
is applied to `/select2`, throwing the same error, so a late tip re-select on an expired
estimate is rejected. **Implementation note (post-review):** the guard lives in the API layer
`API/UI/Select.hs select2'`, *before* `cancelSearchUtil`, gated on `isNothing mbJourneyLegData`
(multimodal legs skip it). Placing it inside domain `select2` would have cancelled the rider's
ongoing search before rejecting; v1 `select` keeps its original in-domain check.

### 7. Seed rules (dev / local)

`dev/feature-migrations/NNNN-tip-module-config.sql` (number picked at implementation
time): `TIP-MODULE-CONFIG` version 1 for the local city — three elements + one
`Unbounded` 100% rollout row. Illustrative program:

```
order 0  {"cat":[{"var":""},{"qarPct":{"if":[{"==":[{"var":"qar"},null]},null,{"*":[100,{"var":"qar"}]}]}}]}
order 1  {"cat":[{"var":""},{"showAfterSec":{"if":[{"==":[{"var":"qarPct"},null]},45,{"if":[{"<":[{"var":"qarPct"},30]},15,{"if":[{"<":[{"var":"qarPct"},60]},30,60]}]}]}}]}
order 2  … repeatIntervalSec (60 / 30 / 45 / 0) and order 3 … maxPrompts (1 / 3 / 2 / 1), same nested shape
```
Note: `json-logic-hs`'s `if` is strictly ternary (`[cond, then, else]`); multi-branch bands
must be nested `if`s. The canonical strings live in `SharedLogic.TipModuleConfig.seedRulesV1Raw`
and are unit-tested through the real engine; the migration is byte-identical to them.

The final object still contains the input keys; `fromJSON` into `TipModuleConfig` ignores
them. Ops-facing rules in production are authored via the dashboard, not migrations.

### Data flow (end to end)

```
BPP on_search (unchanged) ──QAR tag──▶ rider Estimate.qar
UI polls /rideSearch/{id}/results
  └─ per estimate: TipModuleConfigInput{qar,tier,km,isVNP}
       └─ getAppDynamicLogic(city, TIP-MODULE-CONFIG, localTime, toss(searchId)) → version
            └─ runLogics → TipModuleConfig | Nothing
                 └─ <|> RiderConfig.tipModuleConfig → EstimateAPIEntity.tipModuleConfig (+ qar)
UI: select2 → timer(showAfterSec, repeatIntervalSec, maxPrompts) → add tip → select2 again
```

### Error handling

| Situation | Result |
|---|---|
| No rollout / no elements for city | `RiderConfig.tipModuleConfig` |
| Rule evaluation error / output doesn't decode | logged (`logWarning`), `RiderConfig.tipModuleConfig` |
| `estimate.qar` absent (non-VNP BPP, low-demand bucket) | rules receive `qar: null` and must handle it (seed rule shows the pattern); if they return null, RiderConfig default |
| `RiderConfig.tipModuleConfig` also `Nothing` | `tipModuleConfig: null` → UI hides module |
| `select2` on expired estimate | same error as v1 `select` |

Nothing in this path can fail the `/results` or `/select2` request.

## Testing

- **Compile**: `, run-generator` → `, hpack` if needed → `cabal build all` (`-Werror`).
  Rider-app, yudhishthira, and both dashboards (they match on `LogicDomain`) must build.
- **Rule fixture as test**: run the seed rules through the dashboard's
  `postNammaTagAppDynamicLogicVerify` with `qar` = `0.2`, `0.7`, `null` and assert
  `{15,30,3}`, `{60,0,1}`, `{45,60,1}` respectively. Doubles as the ops runbook.
- **Integration** (Postman, `dev/integration-tests/collections/`, Local env), extending
  the ride-search flow:
  1. after `on_search`, `GET /rideSearch/{id}/results`: every estimate has
     `tipModuleConfig` matching the band for the mock BPP's `QAR` tag, and `qar` echoed;
  2. rollout at 0% → `RiderConfig` default returned;
  3. `RiderConfig.tipModuleConfig = null` and rollout 0% → `tipModuleConfig == null`;
  4. two consecutive `/results` polls return identical `tipModuleConfig` (deterministic toss);
  5. `select2` on an expired estimate returns the expiry error.
- **Observability**: enable the JSON-logic debug flag for `(city, TIP-MODULE-CONFIG)` and
  confirm rows in ClickHouse `app_monitor.json_logic_transactions`.

## Files touched (expected)

| File | Change |
|---|---|
| `lib/yudhishthira/src/Lib/Yudhishthira/Types.hs` | `TIP_MODULE_CONFIG` constructor + Show/Read/Enumerable |
| `app/rider-platform/rider-app/Main/src/SharedLogic/TipModuleConfig.hs` | new: input/output types, `getTipModuleConfigFromModel` |
| `app/rider-platform/rider-app/Main/spec/Storage/RiderConfig.yaml` (+ generated) | `tipModuleConfig` JSON column |
| `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Estimate.hs` | `EstimateAPIEntity.tipModuleConfig`, `.qar`; builder wiring |
| `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Quote.hs` (+ other callers) | fetch RiderConfig once, pass default |
| `app/rider-platform/rider-app/Main/src/Domain/Action/Dashboard/NammaTag.hs` | verify + schema arms |
| `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs` | `validTill` guard in `select2` |
| `dev/migrations-read-only/rider-app/rider_config.sql` | generated |
| `dev/feature-migrations/NNNN-tip-module-config.sql` | seed rules + rollout |
| `dev/integration-tests/collections/…` | assertions above |
