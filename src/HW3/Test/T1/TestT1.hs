  
module HW3.Test.T1.TestT1 where   
  import Test.Tasty (defaultMain, testGroup, TestTree, testGroup)
  import Test.Tasty.Hspec (describe, it, shouldBe, testSpec, SpecWith, shouldReturn, Spec, Expectation)
  import Hedgehog (Gen, Property, discover)
  import HW3.Action
  import HW3.Base
  import HW3.Parser
  import HW3.Evaluator
  import HW3.Test.TestUtils
  import Data.Ratio ((%))

  spec :: Spec
  spec = do
    it "simple" $ do
      "0" `evaluatesTo` HiValueNumber 0
      "add(2,2)" `evaluatesTo` HiValueNumber 4
      "sub(1,2)" `evaluatesTo` HiValueNumber (-1)
      "mul(3,3)" `evaluatesTo` HiValueNumber 9
      "div(1,2)" `evaluatesTo` HiValueNumber (1 % 2)
      "div" `evaluatesTo` HiValueFunction HiFunDiv
    it "spaces" $ do
      "      0       " `evaluatesTo` HiValueNumber 0
      " add (  1,  1) " `evaluatesTo` HiValueNumber 2
      "      sub( -100,  3   )" `evaluatesTo` HiValueNumber (-103)
      "mul( 0 ,3)" `evaluatesTo` HiValueNumber 0
    it "brackets" $ do
      "((((((3))))))" `evaluatesTo` HiValueNumber 3
      " ((add))(  ((1)),((1))) " `evaluatesTo` HiValueNumber 2
      "(add)( ((add(1, 3) )  ),mul(3,4))" `evaluatesTo` HiValueNumber 16
    it "parser errors" $ do
      notParse "fun"
      notParse "add[1, 1]"
    it "evaluator errors" $ do
      "add(1)" `failsTo` HiErrorArityMismatch
      "add(1, 1, 1)" `failsTo` HiErrorArityMismatch
      "add(div, mul)" `failsTo` HiErrorInvalidArgument
      "div(1, 0)" `failsTo` HiErrorDivideByZero
      "14(1, 0)" `failsTo` HiErrorInvalidFunction
    it "pretty print" $ do
      "1" `printsTo` "1"
      "div(1, 2)" `printsTo` "0.5"
      "div(1, 3)" `printsTo` "1/3"
      "div(100, 3)" `printsTo` "33+1/3"
      "div(-100, 3)" `printsTo` "-33-1/3"
      "div" `printsTo` "div"

  tests :: IO TestTree
  tests = testSpec "T1" spec

  main :: IO ()
  main = do
    test <- tests
    defaultMain test
