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

spec :: Spec
spec = do
  it "simple functions" $ do
    "length(\"Hello\")" `evaluatesTo` HiValueNumber 5
    "to-upper(\"Hello\")" `evaluatesTo` HiValueString (Text.pack "HELLO")
    "to-lower(\"Hello\")" `evaluatesTo` HiValueString (Text.pack "hello")
    "reverse(\"Hello\")" `evaluatesTo` HiValueString (Text.pack "olleH")
    "trim(\"  Hello   \")" `evaluatesTo` HiValueString (Text.pack "Hello")
    "null" `evaluatesTo` HiValueNull
    "\"hello\"" `evaluatesTo` HiValueString (Text.pack "hello")
    "\"42\"" `evaluatesTo` HiValueString (Text.pack "42")
    "42" `evaluatesTo` HiValueNumber 42
    "\"header\nfooter\"" `evaluatesTo` HiValueString (Text.pack "header\nfooter")

  it "operators and slicing" $ do
    "\"Hello\" + \"World\"" `evaluatesTo` HiValueString (Text.pack "HelloWorld")
    "\"Cat\" * 5" `evaluatesTo` HiValueString (Text.pack "CatCatCatCatCat")
    "\"/home/user\" / \"hi\"" `evaluatesTo` HiValueString (Text.pack "/home/user/hi")
    "\"Hello World\"(0)" `evaluatesTo` HiValueString (Text.pack "H")
    "\"Hello World\"(7)" `evaluatesTo` HiValueString (Text.pack "o")
    "\"Hello World\"(-1)" `evaluatesTo` HiValueNull
    "\"Hello World\"(99)" `evaluatesTo` HiValueNull
    "\"Hello World\"(0, 5)" `evaluatesTo` HiValueString (Text.pack "Hello")
    "\"Hello World\"(2, 4)" `evaluatesTo` HiValueString (Text.pack "ll")
    "\"Hello World\"(1, 5)(0)" `evaluatesTo` HiValueString (Text.pack "e")
  
  it "errors" $ do
    "\"Hello World\"()" `failsTo` HiErrorArityMismatch
    "\"Hello World\"(1, 1, 1)" `failsTo` HiErrorArityMismatch
    "null(1, 1, 1)" `failsTo` HiErrorInvalidFunction
    "\"Hello World\"(-100)(0)" `failsTo` HiErrorInvalidFunction

  it "advanced slicing" $ do
    "\"Hello World\"(0, -4)" `evaluatesTo` HiValueString (Text.pack "Hello W")
    "\"Hello World\"(-4, -1)" `evaluatesTo` HiValueString (Text.pack "orl")
    "\"Hello World\"(2, null)" `evaluatesTo` HiValueString (Text.pack "llo World")
    "\"Hello World\"(null, 5)" `evaluatesTo` HiValueString (Text.pack "Hello")

  it "samples" $ do
    "to-upper(\"what a nice language\")(7, 11)" `evaluatesTo` HiValueString (Text.pack "NICE")
    "\"Hello\" == \"World\"" `evaluatesTo` HiValueBool False
    "length(\"Hello\" + \"World\")" `evaluatesTo` HiValueNumber 10
    "length(\"hehe\" * 5) / 3" `evaluatesTo` HiValueNumber (6 + 2 % 3)
  
  it "printing" $ do
    "\"hello\" + \"world\"" `printsTo` "\"helloworld\""
    "null" `printsTo` "null"


unitTests :: IO TestTree
unitTests = testSpec "unit tests" spec

main :: IO ()
main = do
  unit <- unitTests
  defaultMain $ testGroup "T4" [unit]
