module HW3.Evaluator where
  import HW3.Base (HiError (..), HiValue (..), HiExpr (..), HiFun (..), unaryFunctions, ternaryFunctions, binaryFunctions, binaryArithmeticFunctions, binaryOrdFunctions, binaryLogicFunctions, allFunctions, listFunctions, HiMonad (runAction), actionFunctions, HiAction (..), binaryEqFunctions)
  import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE, runExceptT)
  import Data.Either (isLeft)
  import Control.Monad (when, unless)
  import Text.Megaparsec (choice)
  import Data.Ratio ((%), denominator, numerator)
  import qualified Data.Text
  import Data.Text (pack, Text, unpack)
  import Data.Sequence ( fromList, Seq, empty, singleton, (><), replicate)
  import qualified Data.Sequence
  import Data.Foldable (toList)
  import Data.Word (Word8)
  import Numeric (showHex)
  import qualified Data.ByteString
  import GHC.Char (chr)
  import Data.Text.Encoding (encodeUtf8, decodeUtf8')
  import Codec.Compression.Zlib (compressWith, bestCompression, defaultCompressParams, CompressParams (..), defaultDecompressParams, decompressWith)
  import Data.ByteString.Lazy (toStrict, fromStrict, ByteString)
  import Codec.Serialise (serialise, deserialise, deserialiseOrFail)
  import Data.ByteString (append)
  import qualified Data.List
  import Text.Read (readMaybe)
  import Data.Time (addUTCTime, diffUTCTime)
  import Data.Map (Map, empty, insert, lookup, elems, keys, toList, fromList, singleton, unions, unionsWith)
  import qualified Data.Int

  -- evaluate addition, subtraction, division and multiplication for various hi-types
  hiFunArithmetic :: HiMonad m => HiFun -> HiValue -> HiValue -> ExceptT HiError m HiValue
  -- addition
  hiFunArithmetic HiFunAdd (HiValueNumber x) (HiValueNumber y) = pure $ HiValueNumber $ x + y
  hiFunArithmetic HiFunAdd (HiValueString x) (HiValueString y) = pure $ HiValueString $ Data.Text.append x y
  hiFunArithmetic HiFunAdd (HiValueList x) (HiValueList y) = pure $ HiValueList $ x >< y
  hiFunArithmetic HiFunAdd (HiValueBytes x) (HiValueBytes y) = pure $ HiValueBytes $ Data.ByteString.append x y
  hiFunArithmetic HiFunAdd (HiValueNumber x) (HiValueTime y) = pure $ HiValueTime $ addUTCTime (fromRational x) y
  hiFunArithmetic HiFunAdd y (HiValueNumber x) = hiFunArithmetic HiFunAdd (HiValueNumber x) y
  hiFunArithmetic HiFunAdd _ _ = throwE HiErrorInvalidArgument
  -- subtraction
  hiFunArithmetic HiFunSub (HiValueNumber x) (HiValueNumber y) = pure $ HiValueNumber $ x - y
  hiFunArithmetic HiFunSub (HiValueTime x) (HiValueTime y) = pure $ HiValueNumber $ toRational $ diffUTCTime x y
  hiFunArithmetic HiFunSub _ _ = throwE HiErrorInvalidArgument
  -- multiplication
  hiFunArithmetic HiFunMul (HiValueNumber x) (HiValueNumber y) = pure $ HiValueNumber $ x * y
  hiFunArithmetic HiFunMul y (HiValueNumber x) | denominator x /= 1 || numerator x <= 0 = throwE HiErrorInvalidArgument
  hiFunArithmetic HiFunMul (HiValueString y) (HiValueNumber x) = pure $ HiValueString $ Data.Text.replicate (fromInteger $ numerator x) y
  hiFunArithmetic HiFunMul (HiValueList y) (HiValueNumber x) = pure $ HiValueList $ foldl (><) Data.Sequence.empty (Data.Sequence.replicate (fromInteger $ numerator x) y)
  hiFunArithmetic HiFunMul (HiValueBytes y) (HiValueNumber x) = pure $ HiValueBytes $ Data.ByteString.concat $ Data.List.replicate (fromInteger $ numerator x) y
  hiFunArithmetic HiFunMul _ _ = throwE HiErrorInvalidArgument
  -- division
  hiFunArithmetic HiFunDiv (HiValueNumber x) (HiValueNumber y) = pure $ HiValueNumber $ x / y
  hiFunArithmetic HiFunDiv (HiValueString x) (HiValueString y) = pure $ HiValueString $ Data.Text.append x $ Data.Text.cons '/' y
  hiFunArithmetic HiFunDiv _ _ = throwE HiErrorInvalidArgument

  -- check equivalence of two given values  
  hiFunEq :: (HiMonad m, Eq a) => HiFun -> a -> a -> ExceptT HiError m HiValue
  hiFunEq HiFunEquals x y = pure $ HiValueBool $ x == y
  hiFunEq HiFunNotEquals x y = do
    res <- hiFunEq HiFunEquals x y
    case res of (HiValueBool r) -> return $ HiValueBool $ not r
  hiFunEq _ _ _ = throwE HiErrorInvalidFunction

  -- check ordering of two given values 
  hiFunOrd :: (HiMonad m, Ord a, Eq a) => HiFun -> a -> a -> ExceptT HiError m HiValue
  hiFunOrd HiFunGreaterThan x y    = hiFunOrd HiFunLessThan y x
  hiFunOrd HiFunNotGreaterThan x y = hiFunOrd HiFunNotLessThan y x
  hiFunOrd HiFunLessThan x y       = pure $ HiValueBool $ x < y
  hiFunOrd HiFunNotLessThan x y    = pure $ HiValueBool $ x >= y
  hiFunOrd f x y                   = hiFunEq f x y

  -- evaluate lazy logic operation for the given expressions 
  hiFunLogic :: (HiMonad m) => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
  hiFunLogic HiFunAnd [x, y] = do
    x' <- eval' x
    case x' of
      HiValueBool False -> pure x'
      HiValueNull       -> pure x'
      _                 -> eval' y
  hiFunLogic HiFunOr [x, y]  = do
    x' <- eval' x
    case x' of
      HiValueBool False -> eval' y
      HiValueNull       -> eval' y
      _                 -> pure x'

  -- evaluate list building and folding operations
  hiFunList :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
  hiFunList HiFunRange [x, y] = do
      xx <- eval' x
      yy <- eval' y
      case (xx, yy) of
        (HiValueNumber first, HiValueNumber last) ->
          return $ HiValueList $ Data.Sequence.fromList $ map HiValueNumber [first..last]
        (_, _) -> throwE HiErrorInvalidArgument

  hiFunList HiFunList args = buildList args []
    where 
      -- evaluate a list of given values and build a hi-list of them
      buildList :: HiMonad m => [HiExpr] -> [HiValue] -> ExceptT HiError m HiValue
      buildList [] buf = pure $ HiValueList $ Data.Sequence.fromList $ reverse buf
      buildList (x:xs) buf = do
        xx <- eval' x
        buildList xs (xx:buf)

  hiFunList HiFunFold [x, y] = do
    xx <- eval' x
    yy <- eval' y
    case (xx, yy) of
      (HiValueFunction fun, HiValueList list) ->
        foldList fun (Data.Foldable.toList list)
      (_, _) -> throwE HiErrorInvalidArgument
    where 
      -- fold a list of values using given binary function
      foldList :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
      foldList fun _ | fun `notElem` binaryFunctions = throwE HiErrorInvalidFunction
      foldList fun [] = pure HiValueNull
      foldList fun [x] = pure x
      foldList fun (x:y:xs) = do
        res <- evalBiFunction fun x y
        foldList fun (res:xs)

  hiFunList _ _ = throwE HiErrorInvalidFunction

  -- build hi-action from matching hi-function and given arguments
  hiFunAction :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
  hiFunAction HiFunWrite [file, msg] = do
    msg' <- eval' msg
    file' <- eval' file
    case (msg', file') of
      (HiValueBytes bytes, HiValueString name) -> pure $ HiValueAction $ HiActionWrite (unpack name) bytes
      (HiValueString str, HiValueString name) -> do
        bytes' <- evalUnFunction HiFunEncodeUtf8 (HiValueString str)
        case bytes' of
          HiValueBytes bytes -> pure $ HiValueAction $ HiActionWrite (unpack name) bytes
      _ -> throwE HiErrorInvalidArgument
  hiFunAction HiFunRand [first, last] = do
    first' <- eval' first
    last' <- eval' last
    case (first', last') of
      (HiValueNumber x, HiValueNumber y) ->
        if (denominator x == 1) && (denominator y == 1) 
          && x >= toRational (minBound :: Int) && y >= toRational (minBound :: Int) 
          && x <= toRational (maxBound :: Int) && y <= toRational (maxBound :: Int) 
          && x <= y
          then pure $ HiValueAction $ HiActionRand (fromInteger $ numerator x) (fromInteger $ numerator y)
          else throwE HiErrorInvalidArgument
      (_, _) -> throwE HiErrorInvalidArgument
  hiFunAction f [file] = do
    file' <- eval' file
    case file' of
      HiValueString name -> pure $ HiValueAction $ matchFunWithAction f name
      _                  -> throwE HiErrorInvalidArgument
    where 
      -- build one-argument action from matching hi-fun and text argument
      matchFunWithAction :: HiFun -> Text -> HiAction
      matchFunWithAction HiFunRead file  = HiActionRead $ unpack file
      matchFunWithAction HiFunChDir file = HiActionChDir $ unpack file
      matchFunWithAction HiFunMkDir file = HiActionMkDir $ unpack file
      matchFunWithAction HiFunEcho text  = HiActionEcho text
  hiFunAction _ _ = throwE HiErrorArityMismatch

  -- evaluate binary operatoin for given hi-values
  evalBiFunction :: HiMonad m => HiFun -> HiValue -> HiValue -> ExceptT HiError m HiValue
  evalBiFunction f x y | f `elem` binaryArithmeticFunctions = hiFunArithmetic f x y
  evalBiFunction f (HiValueFunction x) (HiValueFunction y) | f `elem` binaryEqFunctions = hiFunEq f x y
  evalBiFunction f (HiValueBool x) (HiValueNumber y) | f `elem` binaryOrdFunctions = hiFunOrd f (y - 1) y
  evalBiFunction f (HiValueNumber x) (HiValueBool y) | f `elem` binaryOrdFunctions = hiFunOrd f x (x - 1)
  evalBiFunction f x y | f `elem` binaryOrdFunctions = hiFunOrd f x y
  evalBiFunction f x y | f `elem` binaryLogicFunctions = hiFunLogic f [(HiExprValue x), (HiExprValue y)]
  evalBiFunction _ _ _ = throwE HiErrorInvalidArgument

  -- build list of word8 using list of hi-values
  listToWord8 :: HiMonad m => [HiValue] -> ExceptT HiError m [Word8]
  listToWord8 [] = pure []
  listToWord8 ((HiValueNumber x):xs) = if x >= 0 && x < 256 && denominator x == 1
    then do
      rest <- listToWord8 xs
      return $ fromInteger (numerator x):rest
    else throwE HiErrorInvalidArgument
  listToWord8 _ = throwE HiErrorInvalidArgument

  -- build numeric hi-value from given word8
  word8ToHiValue :: Word8 -> HiValue
  word8ToHiValue x = HiValueNumber (fromIntegral x%1)

  -- unite list of maps to one hi-dictionary 
  count' :: [Map HiValue HiValue] -> HiValue
  count' xx = HiValueDict $ Data.Map.unionsWith (\(HiValueNumber x) (HiValueNumber y) -> HiValueNumber (toRational (x + y))) xx

  -- build hi-dictionary of counted entrances for hi-values in given hi-containers
  countEntrances :: HiMonad m => HiValue -> ExceptT HiError m HiValue
  countEntrances (HiValueList lst) =
    pure $ count' $ map (\x -> insert x (HiValueNumber 1) Data.Map.empty ) (Data.Foldable.toList lst)
  countEntrances (HiValueString str) =
    pure $ count' $ map (\x -> insert (HiValueString $ pack (x:"")) (HiValueNumber 1) Data.Map.empty ) (unpack str)
  countEntrances (HiValueBytes bytes) =
    pure $ count' $ map (\x -> insert (word8ToHiValue x) (HiValueNumber 1) Data.Map.empty ) (Data.ByteString.unpack bytes)
  countEntrances _ = throwE HiErrorInvalidArgument

  -- evaluate unary operation 
  evalUnFunction :: HiMonad m => HiFun -> HiValue -> ExceptT HiError m HiValue
  evalUnFunction HiFunNot (HiValueBool x)  = return $ HiValueBool $ not x
  evalUnFunction HiFunLength (HiValueString str) = return $ HiValueNumber $ toRational $ Data.Text.length str
  evalUnFunction HiFunLength (HiValueList lst) = return $ HiValueNumber $ toRational $ Data.Sequence.length lst
  evalUnFunction HiFunToUpper (HiValueString str) = return $ HiValueString $ Data.Text.toUpper str
  evalUnFunction HiFunToLower (HiValueString str) = return $ HiValueString $ Data.Text.toLower str
  evalUnFunction HiFunTrim (HiValueString str) = return $ HiValueString $ Data.Text.strip str
  evalUnFunction HiFunReverse (HiValueString str) = return $ HiValueString $ Data.Text.reverse str
  evalUnFunction HiFunReverse (HiValueList lst) = return $ HiValueList $ Data.Sequence.reverse lst
  evalUnFunction HiFunReverse (HiValueBytes bytes) = return $ HiValueBytes $ Data.ByteString.reverse bytes
  evalUnFunction HiFunPackBytes (HiValueList lst) = do
    res <- listToWord8 $ Data.Foldable.toList lst
    return $ HiValueBytes $ Data.ByteString.pack res
  evalUnFunction HiFunUnpackBytes (HiValueBytes bytes) = return $ HiValueList $ Data.Sequence.fromList $ map word8ToHiValue (Data.ByteString.unpack bytes)
  evalUnFunction HiFunEncodeUtf8 (HiValueString str) = pure $ HiValueBytes $ encodeUtf8 str
  evalUnFunction HiFunDecodeUtf8 (HiValueBytes bytes) = case decodeUtf8' bytes of
    Left _    -> pure HiValueNull
    Right str -> pure $ HiValueString str
  evalUnFunction HiFunZip (HiValueBytes bytes) = pure $ HiValueBytes $ toStrict $ compressWith compressParams $ fromStrict bytes
    where 
      compressParams :: CompressParams
      compressParams = defaultCompressParams { compressLevel = bestCompression }
  evalUnFunction HiFunUnzip (HiValueBytes bytes) = pure $ HiValueBytes $ toStrict $ decompressWith defaultDecompressParams $ fromStrict bytes
  evalUnFunction HiFunSerialise x = pure $ HiValueBytes $ toStrict $ serialise x
  evalUnFunction HiFunDeserialise (HiValueBytes x) = case deserialiseOrFail $ fromStrict x of
    Right res -> pure res
    Left _    -> pure HiValueNull
  evalUnFunction HiFunParseTime (HiValueString str) = case readMaybe $ unpack str of
    Just time -> pure $ HiValueTime time
    Nothing   -> pure HiValueNull
  evalUnFunction HiFunValues (HiValueDict dict) = pure $ HiValueList $ Data.Sequence.fromList $ elems dict
  evalUnFunction HiFunKeys (HiValueDict dict) = pure $ HiValueList $ Data.Sequence.fromList $ keys dict
  evalUnFunction HiFunCount x = countEntrances x
  evalUnFunction HiFunInvert (HiValueDict dict) =
    pure $ HiValueDict $ Data.Map.unionsWith (\(HiValueList x) (HiValueList y) -> HiValueList $ x >< y)
    $ map (\(x, y) -> insert y (HiValueList $ Data.Sequence.singleton x) Data.Map.empty ) (Data.Map.toList dict)
  evalUnFunction _ _ = throwE HiErrorInvalidArgument

  -- evaluate operations on hi-dictionary 
  evalDictFunction :: HiMonad m => [HiExpr] -> Map HiValue HiValue -> ExceptT HiError m HiValue
  evalDictFunction [arg] dict = do
    xx <- eval' arg
    case Data.Map.lookup xx dict of
      Just val -> pure val
      Nothing  -> pure HiValueNull
  evalDictFunction _ _ = throwE HiErrorInvalidFunction

  -- normalise slice borders to the regular form 
  normaliseSlice :: HiMonad m => HiExpr -> HiExpr -> Int -> ExceptT HiError m (Int, Int)
  normaliseSlice first last length = do
    f' <- eval' first
    l' <- eval' last
    case (f', l') of
      (HiValueNumber f, HiValueNumber l) -> do
        unless (denominator f == 1 && denominator l == 1) (throwE HiErrorInvalidArgument)
        let start = if f < 0 then length + fromInteger (numerator f) else fromInteger $ numerator f
        let end = if l < 0 then length + fromInteger (numerator l) else fromInteger $ numerator l
        return (max start 0, min end length)
      (HiValueNull, HiValueNull) -> normaliseSlice (HiExprValue $ HiValueNumber 0) (HiExprValue $ HiValueNumber $ toRational length) length
      (HiValueNull, val) -> normaliseSlice (HiExprValue $ HiValueNumber 0) (HiExprValue val) length
      (val, HiValueNull) -> normaliseSlice (HiExprValue val) (HiExprValue $ HiValueNumber $ toRational length) length
      (_, _) -> throwE HiErrorInvalidArgument

  -- evaluate index 
  normaliseIndex :: HiMonad m => HiExpr -> ExceptT HiError m Int
  normaliseIndex index = do
    ind' <- eval' index
    case ind' of
      (HiValueNumber ind) ->
          if denominator ind == 1
            then pure $ fromInteger (numerator ind)
            else throwE HiErrorInvalidArgument
      _ -> throwE HiErrorInvalidArgument

  -- evaluate index and slicing operations on hi-string
  evalStringFunction :: HiMonad m => [HiExpr] -> Text -> ExceptT HiError m HiValue
  evalStringFunction [arg] str = do
    ind <- normaliseIndex arg
    if ind >= 0 && ind < Data.Text.length str
       then return $ HiValueString $ Data.Text.singleton $  Data.Text.index str ind
      else return HiValueNull
  evalStringFunction [y1, y2] str = do
      (start, end) <- normaliseSlice y1 y2 (Data.Text.length str)
      return $ HiValueString $ Data.Text.drop start (Data.Text.take end str)
  evalStringFunction _ _ = throwE HiErrorInvalidFunction

  -- evaluate index and slicing operations on hi-list
  evalListFunction :: HiMonad m => [HiExpr] -> Seq HiValue -> ExceptT HiError m HiValue
  evalListFunction [arg] list = do
    ind <- normaliseIndex arg
    if ind >= 0 && ind < Data.Sequence.length list
      then return $ Data.Sequence.index list ind
      else return HiValueNull
  evalListFunction [y1, y2] lst = do
      (start, end) <- normaliseSlice y1 y2 (Data.Sequence.length lst)
      return $ HiValueList $ Data.Sequence.drop start (Data.Sequence.take end lst)
  evalListFunction _ _ = throwE HiErrorInvalidFunction

  -- evaluate index and slicing operations on hi-bytestring
  evalBytesFunction :: HiMonad m => [HiExpr] -> Data.ByteString.ByteString -> ExceptT HiError m HiValue
  evalBytesFunction [arg] bytes = do
    ind <- normaliseIndex arg
    if ind >= 0 && ind < Data.ByteString.length bytes
      then return $ word8ToHiValue  $   Data.ByteString.index bytes ind
      else return HiValueNull
  evalBytesFunction [y1, y2] bytes = do
    (start, end) <- normaliseSlice y1 y2 (Data.ByteString.length bytes)
    return $ HiValueBytes $ Data.ByteString.drop start (Data.ByteString.take end bytes)
  evalBytesFunction _ _ = throwE HiErrorInvalidFunction

  -- evaluate given hi-expr using the except monad 
  eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
  eval' (HiExprValue x) = pure x
  eval' (HiExprApply (HiExprValue (HiValueFunction fun)) _) | fun `notElem` allFunctions = throwE HiErrorInvalidFunction
  eval' (HiExprApply (HiExprValue (HiValueFunction fun)) args) | fun `elem` listFunctions = hiFunList fun args
  eval' (HiExprApply (HiExprValue (HiValueFunction fun)) args) | fun `elem` actionFunctions = hiFunAction fun args
  eval' (HiExprApply (HiExprValue (HiValueFunction fun)) args) | fun `elem` binaryLogicFunctions = hiFunLogic fun args
  eval' (HiExprApply (HiExprValue (HiValueFunction fun)) [x, y]) = do
    if fun `notElem` binaryFunctions then throwE HiErrorArityMismatch else do
      xx <- eval' x
      yy <- eval' y
      if fun == HiFunDiv && yy == HiValueNumber 0
        then throwE HiErrorDivideByZero
        else evalBiFunction fun xx yy
  eval' (HiExprApply (HiExprValue (HiValueFunction fun)) [x]) =
    if fun `notElem` unaryFunctions then throwE HiErrorArityMismatch else do
      xx <- eval' x
      evalUnFunction fun xx
  eval' (HiExprApply (HiExprValue (HiValueFunction fun)) [r, x, y]) =
    if fun `notElem` ternaryFunctions then throwE HiErrorArityMismatch else do
      rr <- eval' r
      case rr of
        (HiValueBool res) -> if res then eval' x else eval' y
        _ -> throwE HiErrorInvalidArgument 
  eval' (HiExprApply (HiExprValue (HiValueString str)) args) = evalStringFunction args str
  eval' (HiExprApply (HiExprValue (HiValueList lst)) args) = evalListFunction args lst
  eval' (HiExprApply (HiExprValue (HiValueBytes bytes)) args) = evalBytesFunction args bytes
  eval' (HiExprApply (HiExprValue (HiValueDict dict)) args) = evalDictFunction args dict
  eval' (HiExprApply (HiExprValue (HiValueFunction fun)) _) = throwE HiErrorArityMismatch
  eval' (HiExprApply (HiExprValue _) _) = throwE HiErrorInvalidFunction
  eval' (HiExprApply f args) = do
    ff <- eval' f
    eval' (HiExprApply (HiExprValue ff) args)
  eval' (HiExprRun expr) = do
    expr' <- eval' expr
    case expr' of
      HiValueAction act -> (ExceptT . fmap Right . runAction) act
      _                 -> throwE HiErrorInvalidArgument
  eval' (HiExprDict dict) = HiValueDict <$> buildDict dict
    where 
      -- evaluate a list of given pairs of values and build a hi-dictionary from them
      buildDict :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m (Map HiValue HiValue)
      buildDict [] = pure Data.Map.empty
      buildDict ((k, v):xs) = do
        k' <- eval' k
        v' <- eval' v
        rest <- buildDict xs
        pure $ insert k' v' rest

  -- test :: HiMonad m => m (Either HiError HiValue)
  -- test = runExceptT $ do
  --   (x, y) <- normaliseSlice (HiExprValue $ HiValueNumber 0) (HiExprValue $ HiValueNumber $ -4) 10
  --   return $ HiValueNumber (toRational x)

  -- evaluate hi-expr
  eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
  eval expr = runExceptT (eval' expr)


