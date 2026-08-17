{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE QuasiQuotes #-}

-- | Rider-side JSON-logic domain that turns an estimate's QAR (quote acceptance
-- rate, shipped by the BPP in on_search) into the cadence for the "add tip"
-- module shown during search. Modelled on "SharedLogic.PickupETA".
module SharedLogic.TipModuleConfig
  ( TipModuleConfigInput (..),
    TipModuleConfig (..),
    mkTipModuleConfigInput,
    tipModuleConfigToss,
    fetchTipModuleConfigLogics,
    getTipModuleConfigFromModel,
    resolveTipModuleConfig,
    seedRulesV1Raw,
    seedRulesV1,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Default.Class
import qualified Data.Hashable as DH
import Domain.Types.Estimate (Estimate (..))
import Domain.Types.Extra.RiderConfig (TipModuleConfig (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.RiderConfig (RiderConfig)
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.ServiceTierType as DVST
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Text.RawString.QQ as RS
import Tools.DynamicLogic

-- | Everything the rules may branch on. Kept small on purpose; extend here
-- (and in the dashboard sample via 'Default') when ops need a new dimension.
data TipModuleConfigInput = TipModuleConfigInput
  { qar :: Maybe Double, -- estimate.qar; Nothing when the BPP sent no QAR tag
    serviceTier :: DVST.ServiceTierType,
    estimatedDistanceInKm :: Maybe Double,
    isValueAddNP :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance Default TipModuleConfigInput where
  def =
    TipModuleConfigInput
      { qar = Just 0.5,
        serviceTier = DVST.AUTO_RICKSHAW,
        estimatedDistanceInKm = Just 3.0,
        isValueAddNP = True
      }

mkTipModuleConfigInput :: Bool -> Estimate -> TipModuleConfigInput
mkTipModuleConfigInput isValueAddNP Estimate {..} =
  TipModuleConfigInput
    { qar,
      serviceTier = vehicleServiceTierType,
      estimatedDistanceInKm = (\d -> realToFrac (distanceToHighPrecMeters d) / 1000) <$> estimatedDistance,
      isValueAddNP
    }

-- | Deterministic 1..100 toss per search so every /results poll within one
-- search resolves the same rollout version (same idea as the BPP's
-- poolingLogicVersionToss).
tipModuleConfigToss :: Id DSR.SearchRequest -> Int
tipModuleConfigToss searchReqId = (DH.hash searchReqId.getId `mod` 100) + 1

-- | Resolve the TIP_MODULE_CONFIG logic program once per search request
-- (rollout/toss depend only on city + search id, never on the individual
-- estimate), so the /results hot path does one clock read and one logic lookup
-- for the whole response instead of one per estimate. Never throws: an infra
-- fault degrades to an empty program, which callers read as "no rules".
fetchTipModuleConfigLogics ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    ClickhouseFlow m r
  ) =>
  Seconds ->
  Id DSR.SearchRequest ->
  Id DMOC.MerchantOperatingCity ->
  m [A.Value]
fetchTipModuleConfigLogics timeDiffFromUtc searchReqId merchantOperatingCityId = do
  response <- withTryCatch "fetchTipModuleConfigLogics" $ do
    localTime <- getLocalCurrentTime timeDiffFromUtc
    (allLogics, _mbVersion) <-
      getAppDynamicLogic
        (cast merchantOperatingCityId)
        LYT.TIP_MODULE_CONFIG
        localTime
        Nothing
        (Just $ tipModuleConfigToss searchReqId)
    when (null allLogics) $
      logDebug $ "No TipModuleConfig logics for merchantOperatingCityId: " <> show merchantOperatingCityId
    pure allLogics
  -- 'withTryCatch' already logs the exception itself; this only records which
  -- lookup degraded.
  either
    (\_ -> logError ("Error fetching TipModuleConfig logics for merchantOperatingCityId: " <> show merchantOperatingCityId) >> pure [])
    pure
    response

-- | Run a pre-fetched logic program (see 'fetchTipModuleConfigLogics') against
-- one estimate's input. The rule run and decode sit inside 'withTryCatch': this
-- is the /results hot path, so a fault must degrade to 'Nothing', never
-- propagate to the caller.
getTipModuleConfigFromModel ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    ClickhouseFlow m r
  ) =>
  [A.Value] ->
  Id DSR.SearchRequest ->
  Id DMOC.MerchantOperatingCity ->
  TipModuleConfigInput ->
  m (Maybe TipModuleConfig)
getTipModuleConfigFromModel logics searchReqId merchantOperatingCityId input
  | null logics = pure Nothing
  | otherwise = do
    response <- withTryCatch "getTipModuleConfigFromModel" $ do
      resp <- LYDL.runLogicsWithDebugLog LYDL.Rider (cast merchantOperatingCityId) LYT.TIP_MODULE_CONFIG (Just searchReqId.getId) logics input
      -- runLogics collects per-rule failures here and still succeeds overall.
      unless (null resp.errors) $
        logWarning $ "TipModuleConfig logics reported errors - " <> show resp.errors <> " - input: " <> show input
      case (A.fromJSON resp.result :: A.Result TipModuleConfig) of
        A.Success result -> return (Just result)
        A.Error err -> do
          logWarning $ "Error parsing TipModuleConfig - " <> show err <> " - " <> show resp <> " - input: " <> show input
          return Nothing
    case response of
      -- 'withTryCatch' already logged the exception; keep one line for the input.
      Left _ -> do
        logWarning $ "Error resolving TipModuleConfig - input: " <> show input
        return Nothing
      Right mbConfig -> return mbConfig

-- | Rules first, city default second. Never throws: any failure in the rule
-- evaluation or decoding degrades to the city default, as does an empty logic
-- program.
resolveTipModuleConfig ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    ClickhouseFlow m r
  ) =>
  RiderConfig ->
  [A.Value] ->
  Id DSR.SearchRequest ->
  Id DMOC.MerchantOperatingCity ->
  TipModuleConfigInput ->
  m (Maybe TipModuleConfig)
resolveTipModuleConfig riderConfig logics searchReqId merchantOperatingCityId input =
  (<|> riderConfig.tipModuleConfig) <$> getTipModuleConfigFromModel logics searchReqId merchantOperatingCityId input

-- | Canonical version-1 program: qar% -> band -> cadence. Also the fixture for
-- the unit test and the source of dev/feature-migrations/0049-tip-module-config.sql
-- (keep the raw strings byte-identical to the SQL).
-- Bands: qar absent -> {45,60,1}; <30% -> {15,30,3}; <60% -> {30,45,2}; else {60,0,1}.
--
-- NOTE on the shape of the conditionals: json-logic-hs' @if@ is strictly
-- ternary — @JsonLogic.ifOp@ pattern-matches at most @[cond, then, else]@ and
-- otherwise throws @"wrong number of args supplied, need 3 or less"@ — so the
-- multi-branch bands are written as nested ternary @if@s instead of the flat
-- @[c1,v1,c2,v2,else]@ form that other JsonLogic implementations accept. With
-- the flat form the whole rule fails and the key is silently never set.
seedRulesV1Raw :: [ByteString]
seedRulesV1Raw =
  [ [RS.r|{"cat":[{"var":""},{"qarPct":{"if":[{"==":[{"var":"qar"},null]},null,{"*":[100,{"var":"qar"}]}]}}]}|],
    [RS.r|{"cat":[{"var":""},{"showAfterSec":{"if":[{"==":[{"var":"qarPct"},null]},45,{"if":[{"<":[{"var":"qarPct"},30]},15,{"if":[{"<":[{"var":"qarPct"},60]},30,60]}]}]}}]}|],
    [RS.r|{"cat":[{"var":""},{"repeatIntervalSec":{"if":[{"==":[{"var":"qarPct"},null]},60,{"if":[{"<":[{"var":"qarPct"},30]},30,{"if":[{"<":[{"var":"qarPct"},60]},45,0]}]}]}}]}|],
    [RS.r|{"cat":[{"var":""},{"maxPrompts":{"if":[{"==":[{"var":"qarPct"},null]},1,{"if":[{"<":[{"var":"qarPct"},30]},3,{"if":[{"<":[{"var":"qarPct"},60]},2,1]}]}]}}]}|]
  ]

-- | Decoded form. A rule that fails to parse is dropped; the unit test asserts
-- the length is 4 so a typo cannot go unnoticed.
seedRulesV1 :: [A.Value]
seedRulesV1 = mapMaybe A.decodeStrict seedRulesV1Raw
