{-# LANGUAGE OverloadedStrings #-}

module MainSpec where

import DG.Interpreter (evaluate, initialContext)
import DG.Parser (expression)
import DG.Runtime (Value (..))
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
example expr input expected = do
  case runParser (expression <* eof) "expression" expr of
    Left errors -> Left (errorBundlePretty errors) `shouldBe` expected
    Right program ->
      evaluate (M.insert (S.Identifier "x") (JSON input) initialContext) program `shouldBe` expected

spec :: Spec
spec = parallel $ do
  describe "get" $ do
    specify "x.a in {a:123} is 123" $
      example "x.a" (J.object [("a", J.Number 123)]) (Right [J.Number 123])
    specify "x.a.b in {a:{b:123}} is 123" $ do
      example "x.a.b" (J.object [("a", J.object [("b", J.Number 123)])]) (Right [J.Number 123])
    specify "x.a.z in {a:{b:123}} is empty" $ do
      example "x.a.z" (J.object [("a", J.object [("b", J.Number 123)])]) (Right [])
    specify "x.a.b.c.d.e in {a:123} is empty" $ do
      example "x.a.b.d.e" (J.object [("a", J.Number 123)]) (Right [])
    specify "[1,2,3]" $ do
      example "[1,2,3]" J.Null (Right [J.Array (V.fromList (J.Number <$> [1, 2, 3]))])
    specify "x.b = [a.a.[:]] in {a:[1,2], b:null}" $ do
      example
        "x.b = [\"a\", x.a.[:]]"
        (J.object [("a", J.Array (V.fromList (J.Number <$> [1, 2]))), ("b", J.Null)])
        (Right [J.object [("b", J.Array (V.fromList [J.String "a", J.Number 1, J.Number 2])), ("a", J.Array (V.fromList (J.Number <$> [1, 2])))]])
  describe "set" $ do
    specify "x.a = 'xyz' in {a:null} is {a:'xyz'}" $
      example "x.a = \"xyz\"" (J.object [("a", J.Null)]) (Right [J.object [("a", J.String "xyz")]])
    specify "x.a as z = z * 3 + 1 in {a:4} is {a:13}" $
      example "x.a as z = z * 3 + 1" (J.object [("a", J.Number 4)]) (Right [J.object [("a", J.Number 13)]])
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
  describe "delete" $ do
    specify "x.a = delete in {a:'xyz', b:123} is {b:123}" $
      example "x.a = delete" (J.object [("a", J.String "xyz"), ("b", J.Number 123)]) (Right [J.object [("b", J.Number 123)]])
    specify "x.a.b = delete in {a:{b: 'xyz', c:null}, b:123} is {a:{c:null}, b:123}" $
      example "x.a.b = delete" (J.object [("a", J.object [("b", J.String "xyz"), ("c", J.Null)]), ("b", J.Number 123)]) (Right [J.object [("a", J.object [("c", J.Null)]), ("b", J.Number 123)]])
  describe "boolean expressions" $ do
    specify "!(true or false) == !true and !false" $
      example "!(true or false) == !true and !false" J.Null (Right [J.Bool True])
