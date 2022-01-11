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

spec :: Spec
spec = do
  it "simple operators" $ do
    "1 / 3" `parsesTo` HiExprApply (makeFunction HiFunDiv) 
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber 3]
    "1 * 3" `parsesTo` HiExprApply (makeFunction HiFunMul) 
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber 3]
    "1 + 3" `parsesTo` HiExprApply (makeFunction HiFunAdd) 
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber 3]
    "1 - 3" `parsesTo` HiExprApply (makeFunction HiFunSub) 
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber 3]
    "1 < 3" `parsesTo` HiExprApply (makeFunction HiFunLessThan) 
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber 3]
    "1 > 3" `parsesTo` HiExprApply (makeFunction HiFunGreaterThan) 
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber 3]
    "1 >= 3" `parsesTo` HiExprApply (makeFunction HiFunNotLessThan) 
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber 3]
    "1 <= 3" `parsesTo` HiExprApply (makeFunction HiFunNotGreaterThan) 
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber 3]
    "1 == 3" `parsesTo` HiExprApply (makeFunction HiFunEquals) 
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber 3]
    "1 /= 3" `parsesTo` HiExprApply (makeFunction HiFunNotEquals)
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber 3]
    "1 && 3" `parsesTo` HiExprApply (makeFunction HiFunAnd)
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber 3]
    "1 || 3" `parsesTo` HiExprApply (makeFunction HiFunOr)
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber 3]

  it "brackets and priority" $ do
    "( 1 +     2 *  3 )" `evaluatesTo` HiValueNumber 7
    "  ( (( (1) +     2)) *  3 )" `evaluatesTo` HiValueNumber 9
    notParse "1 < 2 < 3"
    "   ( 1 > if (true, true, false))" `evaluatesTo` HiValueBool True
    " if ( true, mul, add) ( 5 + 5, (1 + 1) * 5 ) " `evaluatesTo` HiValueNumber 100

  it "samples from statements" $ do
    "2 + 2" `evaluatesTo` HiValueNumber 4
    "2 + 2 * 3" `evaluatesTo` HiValueNumber 8
    "(2 + 2) * 3" `evaluatesTo` HiValueNumber 12
    "2 + 2 * 3 == (2 + 2) * 3" `evaluatesTo` HiValueBool False
    "10 == 2*5 && 143 == 11*13" `evaluatesTo` HiValueBool True

unitTests :: IO TestTree
unitTests = testSpec "unit tests" spec

main :: IO ()
main = do
  unit <- unitTests
  defaultMain $ testGroup "T3" [unit]
