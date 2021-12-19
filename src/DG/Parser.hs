{-# LANGUAGE OverloadedStrings #-}

module DG.Parser (expression) where

import Control.Applicative (optional, (<|>))
import qualified DG.Syntax as S
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), Parsec, choice, empty, many, satisfy)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space space1 empty empty)

number :: Parser Int
number = lexeme (L.signed empty L.decimal)

string :: Parser Text
string = lexeme ("\"" *> (T.pack <$> many (try escape <|> satisfy (/= '\"'))) <* "\"")
  where
    escape = "\\" *> choice ['"' <$ "\"", '\r' <$ "r", '\n' <$ "n", '\t' <$ "t"]

identifier :: Parser S.Identifier
identifier = lexeme (S.Identifier . T.pack <$> ((:) <$> letterChar <*> many alphaNumChar))

expression :: Parser S.Expr
expression = do
  value <-
    choice
      [ S.NumLit <$> try number,
        S.StringLit <$> try string,
        S.Variable <$> identifier
      ]

  selection <-
    optional . try $
      "." *> do
        S.Selection value <$> selector <*> pure S.Get

  pure (fromMaybe value selection)

selector :: Parser S.Selector
selector = S.Field <$> identifier
