module HW3.Test.TestUtils
  (
    evaluatesTo
  , evaluateWithPermissions
  , evaluatesWithPermissions
  , failsTo
  , failsWithPermissions
  , throwsPermissionException
  , notParse
  , parsesTo
  , printsTo
  , genHiValue
  , genByteString
  , makeFunction
  ) where

import HW3.Action
import HW3.Base
import HW3.Parser
import HW3.Evaluator
import Test.Tasty.Hspec (Expectation, shouldReturn, runIO)
import Data.Functor.Identity (runIdentity)
import Hedgehog ((===), forAll, property, Gen, Property, check)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import HW3.Pretty (prettyValue)
import Prettyprinter (Doc, Pretty (pretty))
import Prettyprinter.Render.Terminal (AnsiStyle)
import qualified Data.ByteString.Char8 as ByteString
import Data.Char (chr)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Set as Set
import Control.Exception.Base (SomeException)
import Control.Exception (try)

tryParse :: String -> IO (Maybe HiExpr)
tryParse input =
  case parse input of
    Left err -> return Nothing
    Right expr -> return $ Just expr

evaluate :: String -> IO (Maybe (Either HiError HiValue))
evaluate input =
  case parse input of
    Left err -> return Nothing
    Right expr -> do
      result <- eval expr
      return $ Just result

evaluateWithPermissions :: [HiPermission] -> String -> IO (Maybe (Either HiError HiValue))
evaluateWithPermissions permissions input =
  case parse input of
    Left err -> return Nothing
    Right expr -> do
      result <- runHIO (eval expr) (Set.fromList permissions)
      return $ Just result

evaluatesTo :: String -> HiValue -> Expectation
evaluatesTo input expected = do
  evaluate input `shouldReturn` Just (Right expected)

failsTo :: String -> HiError -> Expectation
failsTo input expected = do
  evaluate input `shouldReturn` Just (Left expected)

evaluatesWithPermissions :: [HiPermission] -> String -> HiValue -> Expectation
evaluatesWithPermissions perm input expected = do
  evaluateWithPermissions perm input `shouldReturn` Just (Right expected)

throwsPermissionException :: [HiPermission] -> String -> IO Bool
throwsPermissionException perms expr = do
  case parse expr of
    Left _ -> return False
    Right result -> do
      maybeException <- try $ runHIO (eval result) (Set.fromList perms) 
        :: IO (Either PermissionException (Either HiError HiValue))
      case maybeException of
        Left exp -> return True
        Right _ -> return False

failsWithPermissions :: [HiPermission] -> String -> HiError -> Expectation
failsWithPermissions perm input expected = do
  evaluateWithPermissions perm input `shouldReturn` Just (Left expected)

notParse :: String -> Expectation
notParse input = do
  tryParse input `shouldReturn` Nothing

parsesTo :: String -> HiExpr -> Expectation
parsesTo input expr = do
  tryParse input `shouldReturn` Just expr

printExpr :: String -> IO (Maybe String)
printExpr input = do
  res <- evaluate input
  case res of
    Just (Right value) -> return $ Just (show $ prettyValue value)
    _ -> return Nothing 

printsTo :: String -> String -> Expectation
printsTo expr expect = printExpr expr `shouldReturn` Just expect

genHiValueNumber :: Gen HiValue
genHiValueNumber = do
  x <- Gen.int (Range.linear 0 10)
  return $ HiValueNumber (toRational x)

genHiValueBool :: Gen HiValue
genHiValueBool = Gen.choice
  [ return $ HiValueBool True
  , return $ HiValueBool False]

genString :: Gen HiValue
genString = do
  let listLen = Range.linear 0 100
  array <- Gen.string listLen Gen.enumBounded
  return $ HiValueString $ Text.pack array

genList :: Gen HiValue
genList = do
  let genValue = Gen.choice [genHiValueNumber, genHiValueBool, genByteString, genString]
  let listLen = Range.linear 0 100
  array <- Gen.list listLen genValue
  return $ HiValueList (Seq.fromList array)

genByteString :: Gen HiValue
genByteString = do
  let listLen = Range.linear 0 100
  let byteGen = Gen.int (Range.linear 0 255)
  array <- Gen.list listLen byteGen
  return $ HiValueBytes $ ByteString.pack (map chr array)

genHiValue :: Gen HiExpr
genHiValue = HiExprValue <$> Gen.choice [genHiValueNumber, genHiValueBool, genByteString, genString, genList]

makeFunction :: HiFun -> HiExpr
makeFunction fun = HiExprValue $ HiValueFunction fun
