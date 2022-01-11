module HW3.Pretty where
    import HW3.Base (HiValue (..), HiFun (..), matchFunToString, HiAction(..))
    import Prettyprinter.Render.Terminal (AnsiStyle)
    import Prettyprinter (Doc, Pretty (pretty), (<+>), emptyDoc, dquotes, brackets, slash, comma, space, braces, parens, colon)
    import Data.Scientific (Scientific, formatScientific, FPFormat (Fixed), fromRationalRepetendUnlimited)
    import Data.Ratio (numerator, denominator)
    import Data.Sequence (Seq)
    import Data.Foldable (toList)
    import Data.ByteString (unpack)
    import Data.Word (Word8)
    import Numeric (showHex)
    import qualified Data.Map

    prettyHiList :: [HiValue] -> Doc AnsiStyle
    prettyHiList []     = emptyDoc
    prettyHiList [x]    = prettyValue x
    prettyHiList (x:xs) = prettyValue x <> comma <+> prettyHiList xs

    prettyHiMap :: [(HiValue, HiValue)] -> Doc AnsiStyle
    prettyHiMap []     = emptyDoc
    prettyHiMap [(x, y)]    = prettyValue x <> colon <+> prettyValue y
    prettyHiMap ((x, y):xs) = (prettyValue x <> colon <+> prettyValue y) <> comma <+> prettyHiMap xs

    prettifyByte :: Word8 -> Doc AnsiStyle
    prettifyByte x = pretty $ if length (showHex x "") < 2 then "0" ++ showHex x "" else showHex x ""

    prettyHiBytes :: [Word8] -> Doc AnsiStyle
    prettyHiBytes []     = emptyDoc
    prettyHiBytes [x]    = prettifyByte x
    prettyHiBytes (x:xs) = prettifyByte x <+> prettyHiBytes xs

    prettyFunction :: HiFun -> Doc AnsiStyle
    prettyFunction f = pretty $ matchFunToString f

    prettyAction :: HiAction -> Doc AnsiStyle
    prettyAction HiActionCwd = pretty "cwd"
    prettyAction HiActionNow = pretty "now"
    prettyAction (HiActionRead file) = pretty "read" <> parens (dquotes (pretty file))
    prettyAction (HiActionWrite file bytes) = pretty "write" <> parens (dquotes (pretty file) <> comma <+> prettyValue (HiValueBytes bytes))
    prettyAction (HiActionMkDir file) = pretty "mkdir" <> parens (dquotes (pretty file))
    prettyAction (HiActionChDir file) = pretty "cd" <> parens (dquotes (pretty file))
    prettyAction (HiActionRand x y) = pretty "rand" <> parens (pretty x <> comma <+> pretty y)
    prettyAction (HiActionEcho x) = pretty "echo" <> parens (dquotes (pretty x))

    prettyValue :: HiValue -> Doc AnsiStyle
    prettyValue (HiValueNumber x) = case fromRationalRepetendUnlimited x of
      (y, Nothing) -> pretty $ if denominator x == 1 then show $ numerator x else formatScientific Fixed Nothing y
      (_, Just _) -> (if fst (quotRem (numerator x) (denominator x)) /= 0 then pretty (fst (quotRem (numerator x) (denominator x))) <+> (if x > 0 then pretty "+ " else pretty "- ") else (if x > 0 then emptyDoc else pretty "-"))
        <> pretty (snd (quotRem (abs $ numerator x) (denominator x)))
        <> slash
        <> pretty (denominator x)
    prettyValue (HiValueBool x) = if x then pretty "true" else pretty "false"
    prettyValue (HiValueFunction f) = prettyFunction f
    prettyValue (HiValueString x) = pretty $ show x
    prettyValue HiValueNull = pretty "null"
    prettyValue (HiValueList lst) = brackets $ space <> prettyHiList (toList lst) <> space
    prettyValue (HiValueBytes bytes) = brackets $ pretty "# " <> prettyHiBytes (unpack bytes) <> pretty " #"
    prettyValue (HiValueAction action) = prettyAction action
    prettyValue (HiValueTime time) = pretty  "parse-time" <> parens (dquotes (pretty (show time)))
    prettyValue (HiValueDict dict) = braces $ space <> prettyHiMap (Data.Map.toList dict) <> space
    -- prettyValue x = pretty "unshowable value: " <> pretty (show x)
