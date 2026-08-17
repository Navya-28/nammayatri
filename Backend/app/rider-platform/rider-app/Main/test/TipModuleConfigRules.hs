module TipModuleConfigRules (tipModuleConfigRulesTests) where

import qualified Data.Aeson as A
import Domain.Types.Extra.RiderConfig (TipModuleConfig (..))
import qualified Domain.Types.ServiceTierType as DVST
import JsonLogic (jsonLogicEither)
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import SharedLogic.TipModuleConfig (TipModuleConfigInput (..), seedRulesV1, tipModuleConfigToss)
import Test.Tasty
import Test.Tasty.HUnit

-- Pure re-implementation of Lib.Yudhishthira.Tools.Utils.runLogics: fold the
-- rules left-to-right, each step's output is the next step's input, errors
-- carry the previous object forward. Kept local so the test needs no Flow.
foldRules :: [A.Value] -> A.Value -> A.Value
foldRules rules input = foldl' step input rules
  where
    step acc rule = either (const acc) identity (jsonLogicEither rule acc)

runSeed :: Maybe Double -> A.Result TipModuleConfig
runSeed mbQar =
  A.fromJSON $
    foldRules seedRulesV1 $
      A.toJSON
        TipModuleConfigInput
          { qar = mbQar,
            serviceTier = DVST.AUTO_RICKSHAW,
            estimatedDistanceInKm = Just 3.2,
            isValueAddNP = True
          }

tipModuleConfigRulesTests :: TestTree
tipModuleConfigRulesTests =
  testGroup
    "TipModuleConfig"
    [ testCase "all four seed rules parse" $
        length seedRulesV1 @?= 4,
      testCase "low QAR (0.2) -> early and frequent" $
        runSeed (Just 0.2) @?= A.Success (TipModuleConfig {showAfterSec = 15, repeatIntervalSec = 30, maxPrompts = 3}),
      testCase "mid QAR (0.45) -> moderate" $
        runSeed (Just 0.45) @?= A.Success (TipModuleConfig {showAfterSec = 30, repeatIntervalSec = 45, maxPrompts = 2}),
      testCase "high QAR (0.7) -> late, once" $
        runSeed (Just 0.7) @?= A.Success (TipModuleConfig {showAfterSec = 60, repeatIntervalSec = 0, maxPrompts = 1}),
      testCase "absent QAR -> conservative default branch" $
        runSeed Nothing @?= A.Success (TipModuleConfig {showAfterSec = 45, repeatIntervalSec = 60, maxPrompts = 1}),
      testCase "toss is within 1..100" $
        let tosses = [tipModuleConfigToss (Id (show n)) | n <- [1 .. 500 :: Int]]
         in assertBool "toss out of range" (all (\t -> t >= 1 && t <= 100) tosses),
      testCase "toss golden value for a fixed search id (guards hash-seed drift)" $
        tipModuleConfigToss (Id "search-abc") @?= 55
    ]
