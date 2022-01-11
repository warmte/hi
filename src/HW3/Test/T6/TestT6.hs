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
    "pack-bytes([ 3, 255, 158, 32 ])" `printsTo` "[# 03 ff 9e 20 #]"
    "unpack-bytes([# 10 20 30 #])" `printsTo` "[ 16, 32, 48 ]"
    "encode-utf8(\"Hello!\")" `printsTo` "[# 48 65 6c 6c 6f 21 #]"
    "decode-utf8([# 48 65 6c 6c 6f #])" `printsTo` "\"Hello\""
    "unzip(zip([# 48 65 6c 6c 6f #]))" `printsTo` "[# 48 65 6c 6c 6f #]"
    "deserialise(serialise(5))" `printsTo` "5"
    "deserialise(serialise([1, 2 + 1, 3]))" `printsTo` "[ 1, 3, 3 ]"
    "deserialise(serialise(mul))" `printsTo` "mul"
    "[# 10 #](0)" `printsTo` "16"
    "[# 10 20 30 #](0, 2)" `printsTo` "[# 10 20 #]"

  it "samples" $ do
    "pack-bytes(range(30, 40))" `printsTo` "[# 1e 1f 20 21 22 23 24 25 26 27 28 #]"
    "zip(encode-utf8(\"Hello, World!\" * 1000))" `printsTo` "[# 78 9c ed c7 31 0d 00 20 0c 00 30 2b f0 23 64 0e 30 00 df 92 25 f3 7f a0 82 af fd 1a 37 b3 d6 d8 d5 79 66 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 fc c9 03 ca 0f 3b 28 #]"
    "decode-utf8([# 68 69 #] * 5)" `printsTo` "\"hihihihihi\""
    "unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])" `printsTo` "[# 01 02 03 #]"

propertyTests :: IO TestTree
propertyTests = return $ testGroup "property tests"
  [ testProperty "unzip(zip(A)) ≡ A" $ 
      property $ do
        a <- forAll genByteString
        left <- liftIO $ eval $ 
          HiExprApply (makeFunction HiFunUnzip) [HiExprApply (makeFunction HiFunZip) [HiExprValue a]]
        right <- liftIO $ eval $ HiExprValue a
        left === right
  , testProperty "deserialise(serialise(A)) ≡ A" $ 
      property $ do
        a <- forAll genByteString
        left <- liftIO $ eval $ 
          HiExprApply (makeFunction HiFunDeserialise) [HiExprApply (makeFunction HiFunSerialise) [HiExprValue a]]
        right <- liftIO $ eval $ HiExprValue a
        left === right
  ]

unitTests :: IO TestTree
unitTests = testSpec "unit tests" spec

main :: IO ()
main = do
  unit <- unitTests
  property <- propertyTests
  defaultMain $ testGroup "T6" [unit, property]
