{-# LANGUAGE OverloadedStrings #-}

module MainSpec where

import DG.Interpreter (evaluate)
import DG.Parser (expression)
import qualified DG.Syntax as S
import Data.Aeson (ToJSON, toJSON)
import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Test.Hspec (Spec, describe, hspec, it, parallel, shouldBe, specify)
import Text.Megaparsec (errorBundlePretty, runParser)

main :: IO ()
main = hspec spec

example :: Text -> J.Value -> Either String [J.Value] -> IO ()
example expr input expected = do
  case runParser expression "expression" expr of
    Left errors -> Left (errorBundlePretty errors) `shouldBe` expected
    Right program ->
      evaluate (HM.fromList [(S.Identifier "x", input)]) program `shouldBe` expected

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
  describe "set" $ do
    specify "x.a = 'xyz' in {a:null} is {a:'xyz'}" $
      example "x.a = \"xyz\"" (J.object [("a", J.Null)]) (Right [J.object [("a", J.String "xyz")]])
