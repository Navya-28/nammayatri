module LogicDomainRoundTrip (logicDomainRoundTripTests) where

import Kernel.Prelude
import Lib.Yudhishthira.Types (LogicDomain (..), allValues)
import Test.Tasty
import Test.Tasty.HUnit

logicDomainRoundTripTests :: TestTree
logicDomainRoundTripTests =
  testGroup
    "LogicDomain TIP_MODULE_CONFIG"
    [ testCase "show uses hyphenated DB form" $
        show TIP_MODULE_CONFIG @?= "TIP-MODULE-CONFIG",
      testCase "read of the DB form yields the constructor" $
        readMaybe "TIP-MODULE-CONFIG" @?= Just TIP_MODULE_CONFIG,
      testCase "domain is enumerable (listed for dashboards)" $
        assertBool "TIP_MODULE_CONFIG missing from allValues" (TIP_MODULE_CONFIG `elem` allValues)
    ]
