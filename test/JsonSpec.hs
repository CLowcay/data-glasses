{-# LANGUAGE OverloadedStrings #-}

module JsonSpec where

import DG.Json (JsonE (..), compactPrint, jsonText)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import Data.Void (Void)
import Test.Hspec (Spec, describe, parallel, shouldBe, specify)

shouldParseAs :: String -> JsonE Void -> Spec
shouldParseAs s v = specify ("Should parse " ++ show s) $ A.parseOnly (jsonText <* A.endOfInput) (encodeUtf8 (T.pack s)) `shouldBe` Right v

roundTripsTo :: String -> String -> Spec
roundTripsTo i o =
  specify ("Round trip " ++ show i ++ " to " ++ show o) $
    case A.parseOnly (jsonText <* A.endOfInput) (encodeUtf8 (T.pack i)) of
      Left err -> fail err
      Right j -> LB.toStrict (BB.toLazyByteString (compactPrint j)) `shouldBe` encodeUtf8 (T.pack o)

spec :: Spec
spec = parallel $ do
  describe "json" $ do
    "null" `shouldParseAs` Null
    "true" `shouldParseAs` Bool True
    "false" `shouldParseAs` Bool False
    " \t\n\r\nnull\t\r\n\n  " `shouldParseAs` Null
    "\"\"" `shouldParseAs` String ""
    "\"abc123,./??\"" `shouldParseAs` String "abc123,./??"
    "\"\\\"\"" `shouldParseAs` String "\""
    "\"\\\\\"" `shouldParseAs` String "\\"
    "\"\\r\\n\\t\\b\\//\\f\"" `shouldParseAs` String "\r\n\t\b//\f"
    "\"  \\tabc\\t   \\t  \\t \"" `shouldParseAs` String "  \tabc\t   \t  \t "
    "\"αβγ一二三\"" `shouldParseAs` String "αβγ一二三"
    "\"\\u163442\"" `shouldParseAs` String "\x1634\&42"
    "\"\\u005c\\u005C\"" `shouldParseAs` String "\\\\"
    "\"\\uD834\\uDD1E\"" `shouldParseAs` String "\x1D11E"
    "\"\\uD834\\u0020\"" `shouldParseAs` String (T.pack ['\xD834', ' '])
    "\"\\uD834f\"" `shouldParseAs` String (T.pack ['\xD834', 'f'])
    "\"\\uD834\"" `shouldParseAs` String (T.pack ['\xD834'])
    "01234" `shouldParseAs` Number 1234
    "-654" `shouldParseAs` Number (-654)
    "595.142" `shouldParseAs` Number 595.142
    "0.0000000142" `shouldParseAs` Number 0.0000000142
    "153234500000.0001" `shouldParseAs` Number 153234500000.0001
    "1.2" `shouldParseAs` Number 1.2
    "1.2e3" `shouldParseAs` Number 1.2e3
    "1.2E3" `shouldParseAs` Number 1.2e3
    "1.2e+3" `shouldParseAs` Number 1.2e3
    "1.2e-3" `shouldParseAs` Number 1.2e-3
    "1.111e32" `shouldParseAs` Number 1.111e32
    "1.111e-32" `shouldParseAs` Number 1.111e-32
    "[]" `shouldParseAs` Array (V.fromList [])
    "  [ \r\n\t  \n ]  " `shouldParseAs` Array (V.fromList [])
    " [true]" `shouldParseAs` Array (V.fromList [Bool True])
    " [true  \r\n, false,null]" `shouldParseAs` Array (V.fromList [Bool True, Bool False, Null])
    "[[true],false]" `shouldParseAs` Array (V.fromList [Array (V.fromList [Bool True]), Bool False])
    "{}" `shouldParseAs` Object M.empty
    "\t\t{ \r\n}  " `shouldParseAs` Object M.empty
    "{\"a\":true}" `shouldParseAs` Object (M.fromList [("a", Bool True)])
    "\t{ \"a\"\t:   true }  " `shouldParseAs` Object (M.fromList [("a", Bool True)])
    "{\"a\":true, \"b\":2, \"c\": [{}]}"
      `shouldParseAs` Object (M.fromList [("a", Bool True), ("b", Number 2), ("c", Array (V.fromList [Object M.empty]))])
    "{\"a\":true, \"b\":1, \"a\":false}"
      `shouldParseAs` Object (M.fromList [("a", Bool False), ("b", Number 1)])
    describe "compactPrint" $ do
      "null" `roundTripsTo` "null"
      "  null" `roundTripsTo` "null"
      "true" `roundTripsTo` "true"
      "false" `roundTripsTo` "false"
      "123" `roundTripsTo` "123"
      "123.0" `roundTripsTo` "123"
      "123.456" `roundTripsTo` "123.456"
      "1000000" `roundTripsTo` "1000000"
      "9999999" `roundTripsTo` "9999999"
      "2345662345" `roundTripsTo` "2345662345"
      "23456623450" `roundTripsTo` "23456623450"
      "234566234500" `roundTripsTo` "234566234500"
      "2345662345000" `roundTripsTo` "2345662345000"
      "23456623450000" `roundTripsTo` "2.345662345e13"
      "0.1" `roundTripsTo` "0.1"
      "0.123456" `roundTripsTo` "0.123456"
      "0.012" `roundTripsTo` "0.012"
      "0.0012" `roundTripsTo` "0.0012"
      "0.00012" `roundTripsTo` "1.2e-4"
      "-578" `roundTripsTo` "-578"
      "\"abc\"" `roundTripsTo` "\"abc\""
      "\"a\\\"b\\r\\n\\f\\b\\t\\\\c\"" `roundTripsTo` "\"a\\\"b\\r\\n\\f\\b\\t\\\\c\""
      "\"\\u0000\\u0001\\u0002\\u0003\"" `roundTripsTo` "\"\\u0000\\u0001\\u0002\\u0003\""
      "\" azAZ~.// \"" `roundTripsTo` "\" azAZ~.// \""
      "\"\\u007F\"" `roundTripsTo` "\"\x7F\"" -- Technically this doesn't have to be escaped
      "\"αβγ一二三\"" `roundTripsTo` "\"αβγ一二三\""
      "\"\\u00BD\"" `roundTripsTo` "\"½\""
      "\"\\uD834\\uDD1E\"" `roundTripsTo` "\"\x1D11E\""
      "[ ]" `roundTripsTo` "[]"
      "{ }" `roundTripsTo` "{}"
      "[true, false, null]" `roundTripsTo` "[true,false,null]"
      "{\"a\":[],\r\n\"b\":{\"c\":\r\ntrue}}" `roundTripsTo` "{\"a\":[],\"b\":{\"c\":true}}"
