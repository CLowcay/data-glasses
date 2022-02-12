{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module MainSpec where

import Control.Exception (SomeException, displayException)
import DG.Interpreter (evaluate, initialContext)
import DG.Json (JsonE (..), PureJSON, deannotate)
import DG.Parser (expression)
import DG.Runtime (JsonMeta, Value (..), asJSON)
import qualified DG.Syntax as S
import Data.Bifunctor (first)
import Data.Either (isLeft)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Streamly.Prelude as SL
import Test.Hspec (Spec, describe, hspec, parallel, shouldBe, specify)
import Text.Megaparsec (eof, errorBundlePretty, runParser)

main :: IO ()
main = hspec spec

example :: Text -> JsonE JsonMeta -> Either SomeException [PureJSON] -> IO ()
example expr input expected =
  case runParser (expression <* eof) "expression" expr of
    Left errors -> if isLeft expected then pure () else fail (errorBundlePretty errors)
    Right program ->
      let results = SL.toList (SL.mapMaybe deannotate (asJSON =<< evaluate (M.insert (S.Identifier "x") (JSON input) initialContext) program))
       in first displayException results `shouldBe` first displayException expected

evaluatesTo :: Text -> PureJSON -> Spec
evaluatesTo expr expected = specify (show expr ++ " is " ++ show expected) $ example expr Null (Right [expected])

object :: [(Text, JsonE e)] -> JsonE e
object = Object . M.fromList

array :: [JsonE e] -> JsonE e
array = Array . V.fromList

spec :: Spec
spec = parallel $ do
  describe "get" $ do
    specify "x.a in {a:123} is 123" $
      example "x.a" (object [("a", Number 123)]) (Right [Number 123])
    specify "x.a.b in {a:{b:123}} is 123" $
      example "x.a.b" (object [("a", object [("b", Number 123)])]) (Right [Number 123])
    specify "x.a.z in {a:{b:123}} is empty" $
      example "x.a.z" (object [("a", object [("b", Number 123)])]) (Right [])
    specify "x.a.b.c.d.e in {a:123} is empty" $
      example "x.a.b.d.e" (object [("a", Number 123)]) (Right [])
    specify "[1,2,3]" $
      example "[1,2,3]" Null (Right [array (Number <$> [1, 2, 3])])
    specify "x.b = [a.a[*]] in {a:[1,2], b:null}" $
      example
        "x.b = [\"a\", x.a[*]]"
        (object [("a", array (Number <$> [1, 2])), ("b", Null)])
        (Right [object [("b", array [String "a", Number 1, Number 2]), ("a", array (Number <$> [1, 2]))]])
    specify "x[3:5] in [0,1,2,3,4,5,6,7,8,9]" $
      example "x[3:5]" (array (Number . fromIntegral @Int <$> [0 .. 9])) (Right (Number <$> [3, 4]))
    specify "x[7:] in [0,1,2,3,4,5,6,7,8,9]" $
      example "x[7:]" (array (Number . fromIntegral @Int <$> [0 .. 9])) (Right (Number <$> [7, 8, 9]))
    specify "x[:3] in [0,1,2,3,4,5,6,7,8,9]" $
      example "x[:3]" (array (Number . fromIntegral @Int <$> [0 .. 9])) (Right (Number <$> [0, 1, 2]))
    specify "x['some key$$'] in {'some key$$':123}" $
      example "x[\"some key$$\"]" (object [("some key$$", Number 123)]) (Right [Number 123])
    specify "x['a']['b'] in {a:{b:42}}" $
      example "x[\"a\"][\"b\"]" (object [("a", object [("b", Number 42)])]) (Right [Number 42])
    specify "x[1,3] in [0,1,2,3,4]" $
      example "x[1,3]" (array (Number . fromIntegral @Int <$> [0 .. 4])) (Right [Number 1, Number 3])
    specify "x['a','b'] in {a:1, b:2, c:3}" $
      example
        "x[\"a\", \"b\"]"
        (object [("a", Number 1), ("b", Number 2), ("c", Number 3)])
        (Right [Number 1, Number 2])
    specify "x[*].collect(asArray) in [1,2,3]" $
      let v = array (Number <$> [1, 2, 3])
       in example "x[*].collect(asArray)" v (Right [v])
  describe "set" $ do
    specify "x.a = 'xyz' in {a:null} is {a:'xyz'}" $
      example "x.a = \"xyz\"" (object [("a", Null)]) (Right [object [("a", String "xyz")]])
    specify "x.a as z = z * 3 + 1 in {a:4} is {a:13}" $
      example "x.a as z = z * 3 + 1" (object [("a", Number 4)]) (Right [object [("a", Number 13)]])
    specify "x[*] as z = if z mod 2 == 0 then z + 1 else z - 1 in [1,2,3,4]" $
      example
        "x[*] as z = if z mod 2 == 0 then z + 1 else z - 1"
        (array (Number <$> [1, 2, 3, 4]))
        (Right [array (Number <$> [0, 3, 2, 5])])
  describe "modify" $ do
    specify "x.a += 1 in {a:42}" $
      example "x.a += 1" (object [("a", Number 42)]) (Right [object [("a", Number 43)]])
    specify "x.a -= 1 in {a:42}" $
      example "x.a -= 1" (object [("a", Number 42)]) (Right [object [("a", Number 41)]])
    specify "x.a *= 2 in {a:42}" $
      example "x.a *= 2" (object [("a", Number 42)]) (Right [object [("a", Number 84)]])
    specify "x.a /= 2 in {a:42}" $
      example "x.a /= 2" (object [("a", Number 42)]) (Right [object [("a", Number 21)]])
    specify "x.a ++= 1 in {a:'42'}" $
      example "x.a ++= \"z\"" (object [("a", String "42")]) (Right [object [("a", String "42z")]])
    specify "x[*] *= 2 in {a:1, b:2, c:3}" $
      example
        "x[*] *= 2"
        (object [("a", Number 1), ("a", Number 2), ("a", Number 3)])
        (Right [object [("a", Number 2), ("a", Number 4), ("a", Number 6)]])
  describe "delete" $ do
    specify "x.a = delete in {a:'xyz', b:123} is {b:123}" $
      example "x.a = delete" (object [("a", String "xyz"), ("b", Number 123)]) (Right [object [("b", Number 123)]])
    specify "x.a.b = delete in {a:{b: 'xyz', c:null}, b:123} is {a:{c:null}, b:123}" $
      example "x.a.b = delete" (object [("a", object [("b", String "xyz"), ("c", Null)]), ("b", Number 123)]) (Right [object [("a", object [("c", Null)]), ("b", Number 123)]])
  describe "expressions" $ do
    "(as x in x + 1)(1)" `evaluatesTo` Number 2
    "(as x in (as y in x + y))(1)(2)" `evaluatesTo` Number 3
    "!(true or false) == !true and !false" `evaluatesTo` Bool True
    "-5 mod 3" `evaluatesTo` Number 1
    "-5 - -1" `evaluatesTo` Number (-4)
    "let a = \"1\"; b = \"2\" in a ++ b" `evaluatesTo` String "12"
    "if 1 ==2 then \"a\" else \"b\"" `evaluatesTo` String "b"
    "\"A\" ++ \"B\"" `evaluatesTo` String "AB"
    "[1,2] ++ [3]" `evaluatesTo` array [Number 1, Number 2, Number 3]
