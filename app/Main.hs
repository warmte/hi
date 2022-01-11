{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import System.Console.Haskeline ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT, getExternalPrint )
import HW3.Parser (parse)
import HW3.Evaluator (eval)
import HW3.Pretty ( prettyValue )
import Text.Megaparsec.Error (errorBundlePretty, ParseErrorBundle)
import qualified Data.Set as Set
import HW3.Action (HiPermission (..), HIO (runHIO), PermissionException)
import Data.Set (Set)
import HW3.Base (HiError, HiValue, HiExpr)
import Data.Void (Void)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Exception (catch)

permissions :: String -> Set HiPermission
permissions st = do
  Set.unions [
      if 'r' `elem` st then Set.singleton AllowRead else Set.empty,
      if 'w' `elem` st then Set.singleton AllowWrite else Set.empty,
      if 't' `elem` st then Set.singleton AllowTime else Set.empty
    ]

process :: Set HiPermission -> String -> IO (Either (ParseErrorBundle String Void) (Either HiError HiValue))
process permissions str = do
  mapM (\expr -> runHIO (eval expr) permissions) (parse str)

showResult :: IO (Either (ParseErrorBundle String Void) (Either HiError HiValue)) -> IO String
showResult parsed = do
  either <- parsed
  case either of
    Left errorBundle -> return $ errorBundlePretty errorBundle
    Right expression -> case expression of
      Left error  -> return $ show error
      Right value -> return $ show $ prettyValue value 

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just input -> do
          printer <- getExternalPrint
          lift (catch @PermissionException ((showResult . process (permissions "rwt")) input >>= printer) print)
          loop
