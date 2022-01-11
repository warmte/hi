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

spec :: Spec
spec = do
  it "simple functions" $ do
    "list(1 + 2, 3, \"hello\")" `evaluatesTo` HiValueList 
      (Seq.fromList [HiValueNumber 3, HiValueNumber 3, HiValueString (Text.pack "hello")])
    "range(1, 3.3)" `evaluatesTo` HiValueList 
      (Seq.fromList [HiValueNumber 1, HiValueNumber 2, HiValueNumber 3])
    "fold(add, [11, 22, 33])" `evaluatesTo` HiValueNumber 66
    "fold(mul, [11, 22, 33])" `evaluatesTo` HiValueNumber 7986
    "fold(div, [11, 22, 33])" `evaluatesTo` HiValueNumber (1 % 66)

  it "operators and slicing" $ do
    "length([1, true, \"Hello\"])" `evaluatesTo` HiValueNumber 3
    "reverse([1, true, \"Hello\"])" `evaluatesTo` HiValueList 
      (Seq.fromList [HiValueString (Text.pack "Hello"), HiValueBool True, HiValueNumber 1])
    "[1, 1 + 1] + [3]" `evaluatesTo` HiValueList 
      (Seq.fromList [HiValueNumber 1, HiValueNumber 2, HiValueNumber 3])
    "[1] * 3" `evaluatesTo` HiValueList 
      (Seq.fromList [HiValueNumber 1, HiValueNumber 1, HiValueNumber 1])
    "([1] * 3)(0)" `evaluatesTo` HiValueNumber 1
    "([1] * 3)(0, 2)" `evaluatesTo` HiValueList 
      (Seq.fromList [HiValueNumber 1, HiValueNumber 1])

  it "samples" $ do
    "list(1, 2, 3, 4, 5)" `printsTo` "[ 1, 2, 3, 4, 5 ]"
    "fold(add, [2, 5] * 3)" `printsTo` "21"
    "fold(mul, range(1, 10))" `printsTo` "3628800"
    "[0, true, false, \"hello\", \"world\"](2, 4)" `printsTo` "[ false, \"hello\" ]"
    "reverse(range(0.5, 70/8))" `printsTo` "[ 8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5 ]"


unitTests :: IO TestTree
unitTests = testSpec "unit tests" spec

main :: IO ()
main = do
  unit <- unitTests
  defaultMain $ testGroup "T5" [unit]
