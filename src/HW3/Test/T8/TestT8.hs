import Test.Tasty (defaultMain, testGroup, TestTree, testGroup)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec, SpecWith, shouldReturn, Spec, Expectation)
import HW3.Action
import HW3.Base
import HW3.Parser
import HW3.Evaluator
import TestUtils
import Data.Ratio ((%))
import Control.Monad.IO.Class (liftIO)
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog ((===), forAll, property, Gen, Property, check)
import qualified Data.Text as Text
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Control.Exception.Base (try, SomeException)
import Data.Foldable (toList)

fullPermission :: [HiPermission]
fullPermission = [AllowRead, AllowWrite, AllowTime]

spec :: Spec
spec = do
  it "calculate time" $ do
    "parse-time(\"2021-12-15 00:00:00 UTC\") + 1000" `printsTo` "parse-time(\"2021-12-15 00:16:40 UTC\")"
    "parse-time(\"2021-12-15 00:37:51.000890793 UTC\") - parse-time(\"2021-12-15 00:37:47.649047038 UTC\")"
      `printsTo` "3.351843755"
    "parse-time(\"2021-01-01 00:00:00 UTC\") + 365 * 24 * 60 * 60" `printsTo` 
      "parse-time(\"2022-01-01 00:00:00 UTC\")"

unitTests :: IO TestTree
unitTests = testSpec "unit tests" spec

main :: IO ()
main = do
  unit <- unitTests
  defaultMain $ testGroup "T9" [unit]
