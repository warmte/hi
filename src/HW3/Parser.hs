module HW3.Parser where
  import HW3.Base ( functions,  HiFun(..), HiValue(..), HiExpr(..), HiAction (..) )
  import Data.Void (Void)
  import Text.Megaparsec ( ParseErrorBundle,
    parseTest,
    MonadParsec(try, notFollowedBy),
    manyTill,
    (<|>),
    (<?>),
    parse,
    parseTest,
    between,
    choice,
    many,
    Parsec,
    MonadParsec(eof, label),
    ParseErrorBundle, optional, endBy, sepBy, sepBy1, sepEndBy, satisfy )
  import Control.Monad.Trans.Except (Except, runExcept)
  import Data.Functor.Identity (Identity)
  import Data.Text ( pack )
  import Text.Megaparsec.Char ( string, char, space1, hexDigitChar )
  import Control.Monad.Combinators.Expr ( makeExprParser, Operator (..) )
  import qualified Text.Megaparsec.Char.Lexer as L
  import qualified GHC.TypeLits as Data
  import qualified Data.ByteString
  import Data.Scientific (toRealFloat)
  import Text.Megaparsec.Stream ()
  import Data.Sequence (fromList)
  import Data.Word (Word8)
  import Data.Char (isAlphaNum, isAlpha)

  type Parser = Parsec Void String

  -- space skipping parser 
  skipSpace :: Parser ()
  skipSpace = L.space space1 (L.skipLineComment ";;") (L.skipBlockCommentNested "/*" "*/")

  -- lexeme parser
  lexeme :: Parser a -> Parser a
  lexeme = L.lexeme skipSpace

  -- parser for numeric values
  num :: Parser HiExpr
  num = label "number" $ HiExprValue . HiValueNumber <$> L.signed skipSpace (do toRational <$> L.scientific)

  -- parser for boolean values
  bool :: Parser HiExpr
  bool = label "bool" $ choice [ HiExprValue <$> (HiValueBool True <$ string "true"), HiExprValue <$> (HiValueBool False <$ string "false") ]

  -- parser for null value 
  nullp :: Parser HiExpr
  nullp = label "null" $ HiExprValue <$> (HiValueNull <$ string "null")

  -- parser for function names 
  fun :: Parser HiExpr
  fun = label "function" $ HiExprValue <$> (HiValueFunction <$> choice (map (\(name, fun) -> fun <$ string name) functions))

  -- parser for actions which are not parser as functions
  action :: Parser HiExpr
  action = label "action" $ HiExprValue <$> choice [ HiValueAction HiActionCwd <$ string "cwd", HiValueAction HiActionNow <$ string "now" ]

  -- parser for strings
  stringLiteral :: Parser HiExpr
  stringLiteral = HiExprValue <$> (HiValueString . pack <$> (char '"' >> manyTill L.charLiteral (char '"')))

  -- parser for lists of values 
  list :: Parser HiExpr
  list = do
    elems <- between (lexeme $ char '[') (lexeme $ char ']') args
    pure $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) elems

  -- parser for bytestrings
  bytearray :: Parser HiExpr
  bytearray = label "bytestring" $ do
    elems <- between (lexeme $ string "[#") (lexeme $ string "#]") (sepEndBy byte space1)
    pure $ HiExprValue $ HiValueBytes $ Data.ByteString.pack elems

  -- parser which parses a byte 
  byte :: Parser Word8
  byte = do
    f <- hexDigitChar
    s <- hexDigitChar
    return $ read $ "0x" ++ [f, s]

  -- parser which parses a dictionary key-value element
  dictelem :: Parser (HiExpr, HiExpr)
  dictelem = lexeme $ do
    key <- lexeme hiexpr
    _ <- lexeme $ char ':'
    value <- lexeme hiexpr
    return (key, value)

  -- parser for dictionaries 
  dict :: Parser HiExpr
  dict = label "dictionary" $ HiExprDict <$> between (lexeme $ char '{') (lexeme $ char '}') (sepBy dictelem (lexeme $ char ','))

  -- parser for all values 
  value :: Parser HiExpr
  value = label "value" $ choice [ dict, fun, action, num, bool, nullp, stringLiteral, bytearray, list ]

  -- parser for a list of arguments 
  args :: Parser [HiExpr]
  args = sepBy hiexpr (lexeme $ char ',')

  -- different ways to apply something to a function
  data FuncModifier = Args [HiExpr] | DottedArg HiExpr | Run deriving Show

  -- parser for function arguments 
  funcargs :: Parser FuncModifier
  funcargs = Args <$> between (lexeme $ char '(') (lexeme $ char ')') args

  -- parser for one dotted function argument
  dotarg :: Parser FuncModifier
  dotarg = lexeme $ do
    _ <- lexeme $ char '.'
    text <- (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
    pure $ DottedArg $ HiExprValue $ HiValueString $ pack text

  -- parser for run sign 
  run :: Parser FuncModifier
  run = Run <$ lexeme (char '!')

  -- parser which parses function application
  hiexprapply :: Parser HiExpr
  hiexprapply = label "expression application" $ do
    name <- lexeme (between (lexeme $ char '(') (lexeme $ char ')') hiexpr <|> value)
    funArgs <- many (funcargs <|> run <|> dotarg)
    pure $ if not (Prelude.null funArgs) then combine name funArgs else name

    where
      -- evaluate a result value of all stacked applications 
      combine :: HiExpr -> [FuncModifier] -> HiExpr
      combine x []     = x
      combine x [Args args]    = HiExprApply x args
      combine x [DottedArg arg]    = HiExprApply x [arg]
      combine x [Run]    = HiExprRun x
      combine x ((Args args):ys) = combine (HiExprApply x args) ys
      combine x ((DottedArg arg):ys) = combine (HiExprApply x [arg]) ys
      combine x (Run:ys) = combine (HiExprRun x) ys

  -- parser for parse all the expression except binary operators
  hiexpr' :: Parser HiExpr
  hiexpr' = label "expression" $ choice [ hiexprapply, between (lexeme $ char '(') (lexeme $ char ')') hiexpr ]

  -- main parser
  hiexpr :: Parser HiExpr
  hiexpr = makeExprParser hiexpr' table <?> "table expression"

  -- parser which parses 'operation' symbol which are not followed by the 'notfollow' symbol
  -- we need it to be able to parse '/' operation which is unfortunately a prefix or another operation '/=' with less priority
  op :: String -> String -> Parser String
  op operation notfollow = (lexeme . try) (string operation <* notFollowedBy (lexeme $ string notfollow))

  table :: [[Operator Parser HiExpr]]
  table = [ [ binary InfixL "*"  HiFunMul
          , binary' InfixL  (op "/" "=")  HiFunDiv  ]
          , [ binary InfixL "+"  HiFunAdd
          , binary InfixL "-"  HiFunSub  ]
          , [  binary InfixN "<=" HiFunNotGreaterThan
          , binary InfixN ">=" HiFunNotLessThan
          , binary InfixN ">"  HiFunGreaterThan
          , binary InfixN "<"  HiFunLessThan
          , binary InfixN "==" HiFunEquals
          , binary InfixN "/=" HiFunNotEquals ]
          , [ binary InfixR "&&" HiFunAnd ]
          , [ binary InfixR "||"  HiFunOr  ] ]

  -- construct normal hi-function application by binary operations symbols
  binary :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> String -> HiFun -> Operator Parser HiExpr
  binary infixtype name = binary' infixtype (lexeme $ string name)

  binary' :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> Parser String -> HiFun -> Operator Parser HiExpr
  binary' pri parser fun = pri ((\ x y -> HiExprApply (HiExprValue $ HiValueFunction fun) [x, y]) <$ parser )

  -- parseT :: String -> IO ()
  -- parseT = parseTest hiexpr

  -- main parse function
  parse :: String -> Either (ParseErrorBundle String Void) HiExpr
  parse = Text.Megaparsec.parse (between skipSpace eof hiexpr) ""

