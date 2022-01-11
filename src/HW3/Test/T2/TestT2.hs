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
  it "bool logic" $ do
    "true" `evaluatesTo` HiValueBool True
    "false" `evaluatesTo` HiValueBool False

    "not(true)" `evaluatesTo` HiValueBool False
    "not(false)" `evaluatesTo` HiValueBool True

    "and(false, false)" `evaluatesTo` HiValueBool False
    "and(false, true)" `evaluatesTo` HiValueBool False
    "and(true, false)" `evaluatesTo` HiValueBool False
    "and(true, true)" `evaluatesTo` HiValueBool True

    "or(false, false)" `evaluatesTo` HiValueBool False
    "or(false, true)" `evaluatesTo` HiValueBool True
    "or(true, false)" `evaluatesTo` HiValueBool True
    "or(true, true)" `evaluatesTo` HiValueBool True

  it "compare numbers" $ do
    "less-than(1, 2)" `evaluatesTo` HiValueBool True
    "greater-than(1, 2)" `evaluatesTo` HiValueBool False
    "equals(1, 2)" `evaluatesTo` HiValueBool False
    "equals(1, 1)" `evaluatesTo` HiValueBool True
    "not-less-than(1, 2)" `evaluatesTo` HiValueBool False
    "not-greater-than(1, 2)" `evaluatesTo` HiValueBool True
    "not-equals(1, 2)" `evaluatesTo` HiValueBool True
    "not-equals(2, 2)" `evaluatesTo` HiValueBool False

  it "samples from statements" $ do
    "equals(10, 10)" `evaluatesTo` HiValueBool True
    "equals(false, false)" `evaluatesTo` HiValueBool True
    "equals(3, 10)" `evaluatesTo` HiValueBool False
    "equals(1, true)" `evaluatesTo` HiValueBool False

    "less-than(3, 10)" `evaluatesTo` HiValueBool True
    "less-than(false, true)" `evaluatesTo` HiValueBool True
    "less-than(false, 0)" `evaluatesTo` HiValueBool True

  it "branching" $ do
    "if(true, 1, 2)" `evaluatesTo` HiValueNumber 1
    "if(false, 1, 2)" `evaluatesTo` HiValueNumber 2
    "if(true, 1, true)" `evaluatesTo` HiValueNumber 1
    "if(false, 1, true)" `evaluatesTo` HiValueBool True
    "if(true, add, mul)" `evaluatesTo` HiValueFunction HiFunAdd
    "if(true, add, mul)(10, 10)" `evaluatesTo` HiValueNumber 20
    "if(false, add, mul)(10, 10)" `evaluatesTo` HiValueNumber 100

propertyTests :: IO TestTree
propertyTests = return $ testGroup "property tests"
  [ testProperty "greater-than(A, B) ≡ less-than(B, A)" $ 
      property $ do
        a <- forAll genHiValue
        b <- forAll genHiValue
        left <- liftIO $ eval $ 
          HiExprApply (makeFunction HiFunGreaterThan) [a, b]
        right <- liftIO $ eval $ 
          HiExprApply (makeFunction HiFunLessThan) [b, a]
        left === right
  , testProperty "not-equals(A, B) ≡ not(equals(A, B))" $ 
      property $ do
        a <- forAll genHiValue
        b <- forAll genHiValue
        left <- liftIO $ eval $ 
          HiExprApply (makeFunction HiFunNotEquals) [a, b]
        right <- liftIO $ eval $ 
          HiExprApply (makeFunction HiFunNot) [
            HiExprApply (makeFunction HiFunEquals) [a, b]
          ]
        left === right
  , testProperty "not-less-than(A, B) ≡ not(less-than(A, B))" $ 
      property $ do
        a <- forAll genHiValue
        b <- forAll genHiValue
        left <- liftIO $ eval $ 
          HiExprApply (makeFunction HiFunNotLessThan) [a, b]
        right <- liftIO $ eval $ 
          HiExprApply (makeFunction HiFunNot) [
            HiExprApply (makeFunction HiFunLessThan) [a, b]
          ]
        left === right
  , testProperty "not-greater-than(A, B) ≡ not(greater-than(A, B))" $ 
      property $ do
        a <- forAll genHiValue
        b <- forAll genHiValue
        left <- liftIO $ eval $ 
          HiExprApply (makeFunction HiFunNotGreaterThan) [a, b]
        right <- liftIO $ eval $ 
          HiExprApply (makeFunction HiFunNot) [
            HiExprApply (makeFunction HiFunGreaterThan) [a, b]
          ]
        left === right
  , testProperty "if(true, A, B) ≡ A" $ 
      property $ do
        a <- forAll genHiValue
        b <- forAll genHiValue
        left <- liftIO $ eval $ 
          HiExprApply (makeFunction HiFunIf) [HiExprValue $ HiValueBool True, a, b]
        right <- liftIO $ eval a
        left === right
  , testProperty "if(false, A, B) ≡ B" $ 
      property $ do
        a <- forAll genHiValue
        b <- forAll genHiValue
        left <- liftIO $ eval $ 
          HiExprApply (makeFunction HiFunIf) [HiExprValue $ HiValueBool False, a, b]
        right <- liftIO $ eval b
        left === right
  ]

unitTests :: IO TestTree
unitTests = testSpec "unit tests" spec

main :: IO ()
main = do
  unit <- unitTests
  property <- propertyTests
  defaultMain $ testGroup "T2" [unit, property]
