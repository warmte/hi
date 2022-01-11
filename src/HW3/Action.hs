module HW3.Action where
  import Control.Exception ( Exception, throwIO )
  import Data.Set (Set)
  import HW3.Base (HiMonad (runAction), HiAction (..), HiValue (..))
  import System.Directory (getCurrentDirectory, createDirectory, setCurrentDirectory, doesDirectoryExist, listDirectory, doesFileExist)
  import Control.Monad.IO.Class (MonadIO(liftIO))
  import Data.Text ( pack )
  import Data.Sequence (fromList)
  import Data.Text.Encoding (decodeUtf8')
  import Data.ByteString (readFile, writeFile)
  import Control.Monad (unless)
  import Data.Time (getCurrentTime)
  import Data.Ratio ((%))
  import System.Random (getStdRandom, uniformR)
  
  data HiPermission = AllowRead | AllowWrite | AllowTime deriving (Show, Eq, Ord, Bounded, Enum)

  data PermissionException = PermissionRequired HiPermission deriving Show

  instance Exception PermissionException

  newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

  joinHIO :: HIO (HIO a) -> HIO a
  joinHIO hio = HIO { runHIO = \st -> format st (runHIO hio st) }

  format :: Set HiPermission -> IO (HIO a) -> IO a
  format st x = do
    xx <- x
    runHIO xx st

  instance Monad HIO where
    m >>= f = joinHIO (fmap f m)

  instance Functor HIO where
    fmap f hio = HIO { runHIO = fmap f . runHIO hio }

  instance Applicative HIO where
    pure x = HIO { runHIO = \st -> pure x }
    (<*>) f hio = HIO { runHIO = \st -> runHIO f st <*> runHIO hio st }

  instance HiMonad HIO where
    runAction HiActionCwd = HIO { runHIO = \st -> do
        unless (AllowRead `elem` st) (throwIO $ PermissionRequired AllowRead)
        HiValueString . pack <$> getCurrentDirectory
      }
    runAction HiActionNow = HIO { runHIO = \st -> do
        unless (AllowTime `elem` st) (throwIO $ PermissionRequired AllowTime)
        HiValueString . pack <$> fmap show getCurrentTime
      }
    runAction (HiActionRand left right) = HIO { runHIO = \st -> do
        num <- getStdRandom (uniformR (left, right))
        return $ HiValueNumber $ toRational num
      }
    runAction (HiActionEcho text) = HIO { runHIO = \st -> do
      unless (AllowWrite `elem` st) (throwIO $ PermissionRequired AllowWrite)
      print text
      return HiValueNull 
    }
    runAction (HiActionChDir file) = HIO { runHIO = \st -> do
      unless (AllowRead `elem` st) (throwIO $ PermissionRequired AllowRead)
      setCurrentDirectory file
      return HiValueNull
    }
    runAction (HiActionMkDir file) = HIO { runHIO = \st -> do
      unless (AllowWrite `elem` st) (throwIO $ PermissionRequired AllowWrite)
      isdir <- doesDirectoryExist file
      if isdir
        then do
          return HiValueNull
        else do
          createDirectory file
          return HiValueNull
    }
    runAction (HiActionRead file) = HIO { runHIO = \st -> do
      unless (AllowRead `elem` st) (throwIO $ PermissionRequired AllowRead)
      isdir <- doesDirectoryExist file
      if isdir
        then do
          lst <- listDirectory file
          return $ HiValueList $ fromList $ map (HiValueString . pack) lst
        else do
          isfile <- doesFileExist file
          if isfile
            then do
              content <- Data.ByteString.readFile file
              case decodeUtf8' content of
                Right str -> return $ HiValueString str
                Left err  -> return HiValueNull
            else return HiValueNull
    }
    runAction (HiActionWrite file content) = HIO { runHIO = \st -> do
      unless (AllowWrite `elem` st) (throwIO $ PermissionRequired AllowWrite)
      Data.ByteString.writeFile file content
      return HiValueNull
    }
