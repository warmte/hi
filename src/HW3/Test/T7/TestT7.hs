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
  it "compute 2+2 using files as variables" $ do
    evaluatesWithPermissions fullPermission "mkdir(\"test_dir\")!" HiValueNull
    evaluatesWithPermissions fullPermission "cd(\"test_dir\")!" HiValueNull
    evaluatesWithPermissions fullPermission "write(\"a\", serialise(2))!" HiValueNull
    evaluatesWithPermissions fullPermission "write(\"b\", serialise(2))!" HiValueNull
    evaluatesWithPermissions fullPermission "deserialise(read(\"a\")!) + deserialise(read(\"b\")!)"
      (HiValueNumber 4)
    evaluatesWithPermissions fullPermission "cd(\"../\")!" HiValueNull

  it "read directory" $ do
    evaluatesWithPermissions fullPermission "mkdir(\"test_dir\")!" HiValueNull
    evaluatesWithPermissions fullPermission "cd(\"test_dir\")!" HiValueNull
    evaluatesWithPermissions fullPermission "mkdir(\"read_dir\")!" HiValueNull
    evaluatesWithPermissions fullPermission "cd(\"read_dir\")!" HiValueNull
    evaluatesWithPermissions fullPermission "write(\"a\", \"One\")!" HiValueNull
    evaluatesWithPermissions fullPermission "write(\"b\", \"Two\")!" HiValueNull
    evaluatesWithPermissions fullPermission "write(\"c\", \"Three\")!" HiValueNull
    readResult <- evaluateWithPermissions fullPermission "read(\".\")!"
    case readResult of
      Just (Right (HiValueList list)) -> do
        if Set.fromList (toList list) == Set.fromList 
          [HiValueString (Text.pack "a"), HiValueString (Text.pack "b"), HiValueString (Text.pack "c")]
        then evaluatesWithPermissions fullPermission "cd(\"../\")!" HiValueNull
        else fail "cannot read directory tmp_dir/read_dir"
      _ -> fail "cannot read directory tmp_dir/read_dir"
    
  it "execute without permissions" $ do
    throwsPermissionException [] "write(\"hi.txt\", \"Hi!\")!" `shouldReturn` True
    throwsPermissionException [AllowRead] "write(\"hi.txt\", \"Hi!\")!" `shouldReturn` True
    throwsPermissionException [] "read(\"a\")!" `shouldReturn` True
    throwsPermissionException [AllowWrite] "read(\"a\")!" `shouldReturn` True
  
  it "printing" $ do
    "read(\"hi.txt\")" `printsTo` "read(\"hi.txt\")"
    "write(\"hi.txt\", \"Hi!\")" `printsTo` "write(\"hi.txt\", [# 48 69 21 #])"
    "mkdir(\"dir\")" `printsTo` "mkdir(\"dir\")"
    "cd(\"dir\")" `printsTo` "cd(\"dir\")"

unitTests :: IO TestTree
unitTests = testSpec "unit tests" spec

main :: IO ()
main = do
  unit <- unitTests
  defaultMain $ testGroup "T7" [unit]
