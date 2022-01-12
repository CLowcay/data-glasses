{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module MainSpec where

import DG.Interpreter (evaluate, initialContext)
import DG.Parser (expression)
import DG.Runtime (Value (..), asJSON)
import qualified DG.Syntax as S
import qualified Data.Aeson as J
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Hspec (Spec, describe, hspec, parallel, shouldBe, specify)
import Text.Megaparsec (eof, errorBundlePretty, runParser)

main :: IO ()
main = hspec spec

example :: Text -> J.Value -> Either String [J.Value] -> IO ()
example expr input expected =
  case runParser (expression <* eof) "expression" expr of
    Left errors -> Left (errorBundlePretty errors) `shouldBe` expected
    Right program ->
      (evaluate (M.insert (S.Identifier "x") (JSON input) initialContext) program >>= traverse asJSON) `shouldBe` expected

evaluatesTo :: Text -> J.Value -> Spec
evaluatesTo expr expected = specify (show expr ++ " is " ++ show expected) $ example expr J.Null (Right [expected])

spec :: Spec
spec = parallel $ do
  describe "get" $ do
    specify "x.a in {a:123} is 123" $
      example "x.a" (J.object [("a", J.Number 123)]) (Right [J.Number 123])
    specify "x.a.b in {a:{b:123}} is 123" $
      example "x.a.b" (J.object [("a", J.object [("b", J.Number 123)])]) (Right [J.Number 123])
    specify "x.a.z in {a:{b:123}} is empty" $
      example "x.a.z" (J.object [("a", J.object [("b", J.Number 123)])]) (Right [])
    specify "x.a.b.c.d.e in {a:123} is empty" $
      example "x.a.b.d.e" (J.object [("a", J.Number 123)]) (Right [])
    specify "[1,2,3]" $
      example "[1,2,3]" J.Null (Right [J.Array (V.fromList (J.Number <$> [1, 2, 3]))])
    specify "x.b = [a.a[*]] in {a:[1,2], b:null}" $
      example
        "x.b = [\"a\", x.a[*]]"
        (J.object [("a", J.Array (V.fromList (J.Number <$> [1, 2]))), ("b", J.Null)])
        (Right [J.object [("b", J.Array (V.fromList [J.String "a", J.Number 1, J.Number 2])), ("a", J.Array (V.fromList (J.Number <$> [1, 2])))]])
    specify "x[3:5] in [0,1,2,3,4,5,6,7,8,9]" $
      example "x[3:5]" (J.Array (V.fromList (J.Number . fromIntegral @Int <$> [0 .. 9]))) (Right (J.Number <$> [3, 4]))
    specify "x[7:] in [0,1,2,3,4,5,6,7,8,9]" $
      example "x[7:]" (J.Array (V.fromList (J.Number . fromIntegral @Int <$> [0 .. 9]))) (Right (J.Number <$> [7, 8, 9]))
    specify "x[:3] in [0,1,2,3,4,5,6,7,8,9]" $
      example "x[:3]" (J.Array (V.fromList (J.Number . fromIntegral @Int <$> [0 .. 9]))) (Right (J.Number <$> [0, 1, 2]))
    specify "x['some key$$'] in {'some key$$':123}" $
      example "x[\"some key$$\"]" (J.object [("some key$$", J.Number 123)]) (Right [J.Number 123])
    specify "x['a']['b'] in {a:{b:42}}" $
      example "x[\"a\"][\"b\"]" (J.object [("a", J.object [("b", J.Number 42)])]) (Right [J.Number 42])
    specify "x[1,3] in [0,1,2,3,4]" $
      example "x[1,3]" (J.Array (V.fromList (J.Number . fromIntegral @Int <$> [0 .. 4]))) (Right [J.Number 1, J.Number 3])
    specify "x['a','b'] in {a:1, b:2, c:3}" $
      example
        "x[\"a\", \"b\"]"
        (J.object [("a", J.Number 1), ("b", J.Number 2), ("c", J.Number 3)])
        (Right [J.Number 1, J.Number 2])
    specify "x[*].collect(asArray) in [1,2,3]" $
      let v = J.Array (V.fromList (J.Number <$> [1, 2, 3]))
       in example "x[*].collect(asArray)" v (Right [v])
  describe "set" $ do
    specify "x.a = 'xyz' in {a:null} is {a:'xyz'}" $
      example "x.a = \"xyz\"" (J.object [("a", J.Null)]) (Right [J.object [("a", J.String "xyz")]])
    specify "x.a as z = z * 3 + 1 in {a:4} is {a:13}" $
      example "x.a as z = z * 3 + 1" (J.object [("a", J.Number 4)]) (Right [J.object [("a", J.Number 13)]])
    specify "x[*] as z = if z mod 2 == 0 then z + 1 else z - 1 in [1,2,3,4]" $
      example
        "x[*] as z = if z mod 2 == 0 then z + 1 else z - 1"
        (J.Array (V.fromList (J.Number <$> [1, 2, 3, 4])))
        (Right [J.Array (V.fromList (J.Number <$> [0, 3, 2, 5]))])
  describe "modify" $ do
    specify "x.a += 1 in {a:42}" $
      example "x.a += 1" (J.object [("a", J.Number 42)]) (Right [J.object [("a", J.Number 43)]])
    specify "x.a -= 1 in {a:42}" $
      example "x.a -= 1" (J.object [("a", J.Number 42)]) (Right [J.object [("a", J.Number 41)]])
    specify "x.a *= 2 in {a:42}" $
      example "x.a *= 2" (J.object [("a", J.Number 42)]) (Right [J.object [("a", J.Number 84)]])
    specify "x.a /= 2 in {a:42}" $
      example "x.a /= 2" (J.object [("a", J.Number 42)]) (Right [J.object [("a", J.Number 21)]])
    specify "x.a ++= 1 in {a:'42'}" $
      example "x.a ++= \"z\"" (J.object [("a", J.String "42")]) (Right [J.object [("a", J.String "42z")]])
    specify "x[*] *= 2 in {a:1, b:2, c:3}" $
      example
        "x[*] *= 2"
        (J.object [("a", J.Number 1), ("a", J.Number 2), ("a", J.Number 3)])
        (Right [J.object [("a", J.Number 2), ("a", J.Number 4), ("a", J.Number 6)]])
  describe "delete" $ do
    specify "x.a = delete in {a:'xyz', b:123} is {b:123}" $
      example "x.a = delete" (J.object [("a", J.String "xyz"), ("b", J.Number 123)]) (Right [J.object [("b", J.Number 123)]])
    specify "x.a.b = delete in {a:{b: 'xyz', c:null}, b:123} is {a:{c:null}, b:123}" $
      example "x.a.b = delete" (J.object [("a", J.object [("b", J.String "xyz"), ("c", J.Null)]), ("b", J.Number 123)]) (Right [J.object [("a", J.object [("c", J.Null)]), ("b", J.Number 123)]])
  describe "expressions" $ do
    "(as x in x + 1)(1)" `evaluatesTo` J.Number 2
    "(as x in (as y in x + y))(1)(2)" `evaluatesTo` J.Number 3
    "!(true or false) == !true and !false" `evaluatesTo` J.Bool True
    "-5 mod 3" `evaluatesTo` J.Number 1
    "-5 - -1" `evaluatesTo` J.Number (-4)
    "let a = \"1\"; b = \"2\" in a ++ b" `evaluatesTo` J.String "12"
    "if 1 ==2 then \"a\" else \"b\"" `evaluatesTo` J.String "b"
    "\"A\" ++ \"B\"" `evaluatesTo` J.String "AB"
    "[1,2] ++ [3]" `evaluatesTo` J.Array (V.fromList [J.Number 1, J.Number 2, J.Number 3])
