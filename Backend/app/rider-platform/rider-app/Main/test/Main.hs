import Kernel.Prelude
import Test.Tasty
import TipModuleConfigRules (tipModuleConfigRulesTests)

main :: IO ()
main = defaultMain $ testGroup "rider-app" [tipModuleConfigRulesTests]
