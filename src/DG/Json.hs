{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module DG.Json (JsonE (..), bom, value, string, PureJSON, jsonText, compactPrint) where

import Control.Applicative (optional)
import Control.Monad (void)
import Data.Attoparsec.ByteString ((<?>))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as CA
import Data.Bifunctor (first)
import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific, scientific, toDecimalDigits)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void, absurd)
import Debug.Trace (traceShow)
import Prelude hiding (String, exp, null)

-- | A value that is either JSON or some other type e.
data JsonE e
  = Null
  | Boolean Bool
  | Number Scientific
  | String Text
  | Array (Vector (JsonE e))
  | -- | Try to maintain the order just in case it matters, but also create a
    -- lazy index to accelerate lookups
    Object (Vector (Text, JsonE e)) ObjectIndex
  | -- | Extension point for non-JSON values
    Extention e
  deriving (Eq, Show)

type PureJSON = JsonE Void

type ObjectIndex = (Map Text [Int])

-- | The UTF-8 Byte-order-mark.  RFC7159 says that we are permitted to ignore
-- this if it appears at the start of the JSON stream, but we are not permitted
-- to append a BOM on output.
bom :: A.Parser ()
bom = () <$ A.string (B.pack [0xEF, 0xBB, 0xBF])

-- | RFC7159 section 2.
jsonText :: A.Parser (JsonE e)
jsonText = optional bom *> whitespace *> value <* whitespace

-- | RFC7159 section 2
beginArray, beginObject, endArray, endObject, nameSeparator, valueSeparator :: A.Parser ()
beginArray = () <$ whitespace <* A.word8 0x5B <* whitespace <?> "'['"
beginObject = () <$ whitespace <* A.word8 0x7B <* whitespace <?> "'{'"
endArray = () <$ whitespace <* A.word8 0x5D <* whitespace <?> "']'"
endObject = () <$ whitespace <* A.word8 0x7D <* whitespace <?> "'}'"
nameSeparator = () <$ whitespace <* A.word8 0x3A <* whitespace <?> "':'"
valueSeparator = () <$ whitespace <* A.word8 0x2C <* whitespace <?> "','"

-- | RFC7159 section 2
whitespace :: A.Parser ByteString
whitespace = A.takeWhile (\c -> c == 0x20 || c == 0x09 || c == 0x0A || c == 0x0D) <?> "whitespace"

-- | RFC7159 section 3
value :: A.Parser (JsonE a)
value =
  A.choice
    [ false,
      null,
      true,
      object <&> \o -> Object o (index o),
      Array <$> array,
      Number <$> number,
      String <$> string
    ]
    <?> "value"

-- | RFC7159 section 3
false :: A.Parser (JsonE a)
false = Boolean False <$ A.string "false" <?> "false"

-- | RFC7159 section 3
null :: A.Parser (JsonE a)
null = Null <$ A.string "null" <?> "null"

-- | RFC7159 section 3
true :: A.Parser (JsonE a)
true = Boolean True <$ A.string "true" <?> "true"

-- | RFC7159 section 4
object :: A.Parser (Vector (Text, JsonE a))
object = beginObject *> (V.fromList <$> (member `A.sepBy` valueSeparator)) <* endObject <?> "object"
  where
    member = (,) <$> (string <* nameSeparator) <*> value <?> "member"

-- | RFC7159 section 5
array :: A.Parser (Vector (JsonE a))
array = beginArray *> (V.fromList <$> value `A.sepBy` valueSeparator) <* endArray <?> "array"

-- | RFC7159 section 6
number :: A.Parser Scientific
number =
  do
    sign <- A.option 1 ((-1) <$ minus)
    int <- CA.decimal
    frac <- optional (A.match (decimalPoint *> CA.signed CA.decimal))
    exp <- optional (e *> CA.signed CA.decimal)
    pure $ case frac of
      Nothing -> scientific (sign * int) (fromMaybe 0 exp)
      Just (raw, f) ->
        let fracDigits = B.length raw
         in scientific (sign * (int * 10 ^ (fracDigits - 1) + f)) (fromMaybe 0 exp - fracDigits + 1)
    <?> "number"
  where
    decimalPoint = A.word8 0x2E <?> "'.'"
    e = A.satisfy (\c -> c == 0x65 || c == 0x45) <?> "'e' or 'E'"
    minus = A.word8 0x2D <?> "'-'"

-- | RFC7159 section 7
string :: A.Parser Text
string = quotationMark *> chars <* quotationMark <?> "string"
  where
    chars =
      T.concat
        <$> A.many'
          ( A.choice
              [ T.decodeUtf8With T.lenientDecode <$> A.takeWhile1 unescaped,
                escape
              ]
          )
    unescaped c = c >= 0x20 && c /= 0x22 && c /= 0x5C
    quotationMark = A.word8 0x22 <?> "'\"'"
    escape =
      A.word8 0x5C
        *> A.choice
          [ "\"" <$ A.word8 0x22,
            "\\" <$ A.word8 0x5C,
            "/" <$ A.word8 0x2F,
            "\b" <$ A.word8 0x62,
            "\f" <$ A.word8 0x66,
            "\n" <$ A.word8 0x6E,
            "\r" <$ A.word8 0x72,
            "\t" <$ A.word8 0x74,
            do
              void (A.word8 0x75) -- u
              sa <- word16Hex
              -- Are we looking at the first codeword of a UTF16 surrogate pair?
              if sa < 0xD800 || sa > 0xDBFF
                then pure (T.singleton (toEnum sa))
                else do
                  -- Check if the next character is also a Unicode escape
                  sbm <- optional (A.word8 0x5C *> A.word8 0x75 *> word16Hex)
                  pure $ case sbm of
                    Nothing -> T.singleton (toEnum sa)
                    Just sb ->
                      if sb < 0xDC00 || sb > 0xDFFF
                        then -- Invalid surrogate pair, so preserve the two code words.
                          T.pack [toEnum sa, toEnum sb]
                        else -- Assemble the surrogate pair into a single Char
                          T.singleton (toEnum (((sa - 0xD800) `shiftL` 10) .|. (sb - 0xDC00) .|. 0x10000))
          ]
    hexit =
      A.choice
        [ subtract 0x30 <$> A.satisfy (\c -> c >= 0x30 && c <= 0x39),
          subtract 0x37 <$> A.satisfy (\c -> c >= 0x41 && c <= 0x46),
          subtract 0x57 <$> A.satisfy (\c -> c >= 0x61 && c <= 0x66)
        ]
        <?> "hexadecimal digit"
    word16Hex = do
      x1 <- fromIntegral <$> hexit
      x2 <- fromIntegral <$> hexit
      x3 <- fromIntegral <$> hexit
      x4 <- fromIntegral <$> hexit
      pure (x1 `shiftL` 12 .|. x2 `shiftL` 8 .|. x3 `shiftL` 4 .|. x4)

-- | Index a JSON object
index :: Vector (Text, JsonE e) -> ObjectIndex
index o = M.fromListWith (++) (V.toList (V.imap (\i (l, _) -> (l, [i])) o))

-- | Print JSON in a compact format
compactPrint :: PureJSON -> BB.Builder
compactPrint = \case
  Null -> "null"
  Boolean True -> "true"
  Boolean False -> "false"
  Number n -> printNumber n
  String txt -> printString txt
  Array a -> "[" <> compactArray a <> "]"
  Object o _ -> "{" <> compactObject o <> "}"
  Extention v -> absurd v
  where
    compactArray a = mconcat (intersperse "," (compactPrint <$> V.toList a))
    compactObject o = mconcat (intersperse "," (V.toList o <&> \(l, v) -> printString l <> ":" <> compactPrint v))

printNumber :: Scientific -> BB.Builder
printNumber n =
  (if n < 0 then "-" else "")
    <> ( if (n > 9999999 || n <= 0x01) && (exp >= digits + 4 || exp < (-2))
           then formatScientific
           else traceShow (mantissa, exp, digits) formatDecimal
       )
  where
    (mantissa, exp) = first (abs <$>) (toDecimalDigits n)
    digits = length mantissa
    formatDecimal
      | exp <= 0 = "0." <> mconcat (digit <$> (replicate (- exp) 0 ++ mantissa))
      | exp >= digits = mconcat (digit <$> (mantissa ++ replicate (exp - digits) 0))
      | otherwise = mconcat (insertAt exp "." (digit <$> mantissa))
    formatScientific = case mantissa of
      [] -> "0"
      [d] -> digit d <> ".0e" <> BB.string7 (show (exp - 1))
      (d : ds) -> digit d <> "." <> mconcat (digit <$> ds) <> "e" <> BB.string7 (show (exp - 1))
    insertAt i x xs = let (a, b) = splitAt i xs in a ++ [x] ++ b
    digit = BB.word8 . (+ 0x30) . fromIntegral

printString :: Text -> BB.Builder
printString s = "\"" <> BB.stringUtf8 (makePrintable =<< T.unpack s) <> "\""
  where
    makePrintable = \case
      '\r' -> "\\r"
      '\n' -> "\\n"
      '\f' -> "\\f"
      '\b' -> "\\b"
      '\t' -> "\\t"
      '\\' -> "\\\\"
      '"' -> "\\\""
      c -> let cp = fromEnum c in if cp < 0x20 then formatHex16 cp else [c]
    formatHex16 i = ['\\', 'u', formatHex (i `shiftR` 12), formatHex (i `shiftR` 8), formatHex (i `shiftR` 4), formatHex i]
    formatHex i = let i' = i .&. 0xF in toEnum (if i' < 0xA then 0x30 + i' else 0x41 + i' - 0xA)
