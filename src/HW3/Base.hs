{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module HW3.Base where
  import Data.Text (Text)
  import Data.Sequence (Seq)
  import Data.ByteString (ByteString)
  import Codec.Serialise (Serialise (..))
  import GHC.Generics (Generic)
  import Data.Time (UTCTime)
  import Data.Map (Map, fromList, (!))
  
  class Monad m => HiMonad m where
    runAction :: HiAction -> m HiValue

  data HiAction
    = HiActionRead  FilePath
    | HiActionWrite FilePath ByteString
    | HiActionMkDir FilePath
    | HiActionChDir FilePath
    | HiActionCwd
    | HiActionNow
    | HiActionRand Int Int
    | HiActionEcho Text
    deriving (Show, Eq, Ord, Serialise, Generic)

  data HiFun     -- function names (e.g. div, sort, length, ...)
    = HiFunDiv
    | HiFunMul
    | HiFunAdd
    | HiFunSub
    | HiFunNot
    | HiFunAnd
    | HiFunOr
    | HiFunLessThan
    | HiFunGreaterThan
    | HiFunEquals
    | HiFunNotLessThan
    | HiFunNotGreaterThan
    | HiFunNotEquals
    | HiFunIf
    | HiFunLength
    | HiFunToUpper
    | HiFunToLower
    | HiFunReverse
    | HiFunTrim
    | HiFunList
    | HiFunRange
    | HiFunFold
    | HiFunPackBytes
    | HiFunUnpackBytes
    | HiFunEncodeUtf8
    | HiFunDecodeUtf8
    | HiFunZip
    | HiFunUnzip
    | HiFunSerialise
    | HiFunDeserialise
    | HiFunRead
    | HiFunWrite
    | HiFunMkDir
    | HiFunChDir
    | HiFunParseTime
    | HiFunRand
    | HiFunEcho
    | HiFunCount
    | HiFunKeys
    | HiFunValues
    | HiFunInvert
    deriving (Show, Eq, Ord, Serialise, Generic)

  data HiValue
    = HiValueNumber Rational
    | HiValueFunction HiFun
    | HiValueBool Bool
    | HiValueNull
    | HiValueString Text
    | HiValueList (Seq HiValue)
    | HiValueBytes ByteString
    | HiValueAction HiAction
    | HiValueTime UTCTime
    | HiValueDict (Map HiValue HiValue)
    deriving (Show, Eq, Ord, Serialise, Generic) -- values (numbers, booleans, strings, ...)

  data HiExpr
    = HiExprValue HiValue
    | HiExprApply HiExpr [HiExpr]
    | HiExprRun HiExpr
    | HiExprDict [(HiExpr, HiExpr)]
    deriving (Show, Eq)    -- expressions (literals, function calls, ...)

  data HiError
    = HiErrorInvalidArgument
    | HiErrorInvalidFunction
    | HiErrorArityMismatch
    | HiErrorDivideByZero
    | Debug String
    deriving (Show, Eq)   -- evaluation errors (invalid arguments, ...)

  -- list of all functions and their string representations
  functions :: [(String, HiFun)]
  functions = [
      ("add", HiFunAdd),
      ("sub", HiFunSub),
      ("mul", HiFunMul),
      ("div", HiFunDiv),
      ("if", HiFunIf),
      ("or", HiFunOr),
      ("and", HiFunAnd),
      ("not-equals", HiFunNotEquals ),
      ("not-greater-than", HiFunNotGreaterThan ),
      ("not-less-than", HiFunNotLessThan),
      ("not", HiFunNot),
      ("equals", HiFunEquals),
      ("greater-than", HiFunGreaterThan),
      ("less-than", HiFunLessThan),
      ("length", HiFunLength),
      ("reverse", HiFunReverse),
      ("to-upper", HiFunToUpper),
      ("to-lower", HiFunToLower),
      ("trim", HiFunTrim),
      ("list", HiFunList),
      ("range", HiFunRange),
      ("fold", HiFunFold),
      ("pack-bytes", HiFunPackBytes),
      ("unpack-bytes", HiFunUnpackBytes),
      ("zip", HiFunZip),
      ("unzip", HiFunUnzip),
      ("encode-utf8", HiFunEncodeUtf8),
      ("decode-utf8", HiFunDecodeUtf8),
      ("serialise", HiFunSerialise),
      ("deserialise", HiFunDeserialise),
      ("read", HiFunRead),
      ("write", HiFunWrite),
      ("mkdir", HiFunMkDir),
      ("cd", HiFunChDir),
      ("parse-time", HiFunParseTime),
      ("rand", HiFunRand),
      ("echo", HiFunEcho),
      ("count", HiFunCount),
      ("keys", HiFunKeys),
      ("values", HiFunValues),
      ("invert", HiFunInvert)
    ]

  functionsMap :: Map HiFun String 
  functionsMap = Data.Map.fromList (map (\(x, y) -> (y, x)) functions)

  -- return string representation of the function
  matchFunToString :: HiFun -> String
  matchFunToString fun = functionsMap!fun

  -- list of functions working with bytestrings 
  byteFunctions :: [HiFun]
  byteFunctions = [HiFunPackBytes, HiFunUnpackBytes, HiFunEncodeUtf8, HiFunDecodeUtf8, HiFunZip, HiFunUnzip, HiFunSerialise, HiFunDeserialise]

  -- list of functions working with dicts
  dictFunctions :: [HiFun]
  dictFunctions = [HiFunCount, HiFunKeys, HiFunValues, HiFunInvert]

  -- list of functions with one argument
  unaryFunctions :: [HiFun]
  unaryFunctions = [HiFunNot, HiFunLength, HiFunToUpper, HiFunToLower, HiFunTrim, HiFunReverse, HiFunParseTime] ++ byteFunctions ++ dictFunctions

  -- list of functions working with arithmetic operations
  binaryArithmeticFunctions :: [HiFun]
  binaryArithmeticFunctions = [HiFunAdd, HiFunSub, HiFunMul, HiFunDiv]

  -- list of functions working with equality operations
  binaryEqFunctions ::[HiFun]
  binaryEqFunctions = [HiFunEquals, HiFunNotEquals]

  -- list of functions working with ordering operations
  binaryOrdFunctions ::[HiFun]
  binaryOrdFunctions = [HiFunLessThan, HiFunNotLessThan, HiFunGreaterThan, HiFunNotGreaterThan] ++ binaryEqFunctions

  -- list of functions working with logic 
  binaryLogicFunctions :: [HiFun]
  binaryLogicFunctions = [HiFunAnd, HiFunOr]

  -- list of functions with two arguments
  binaryFunctions :: [HiFun]
  binaryFunctions = binaryArithmeticFunctions ++ binaryOrdFunctions ++ binaryLogicFunctions 

  -- list of functions with three arguments
  ternaryFunctions :: [HiFun]
  ternaryFunctions = [HiFunIf]

  -- list of functions working with lists
  listFunctions :: [HiFun]
  listFunctions = [HiFunList, HiFunFold, HiFunRange]

  -- list of functions matching with actions
  actionFunctions :: [HiFun]
  actionFunctions = [HiFunRead, HiFunWrite, HiFunMkDir, HiFunChDir, HiFunRand, HiFunEcho]

  -- list of all functions
  allFunctions :: [HiFun]
  allFunctions = unaryFunctions ++ binaryFunctions ++ ternaryFunctions ++ listFunctions ++ actionFunctions
