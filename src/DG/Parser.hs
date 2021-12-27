{-# LANGUAGE OverloadedStrings #-}

module DG.Parser (expression) where

import Control.Applicative (optional, (<|>))
import qualified DG.Syntax as S
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), Parsec, between, choice, empty, many, satisfy, sepBy)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space space1 empty empty)

boolean :: Parser Bool
boolean = choice [True <$ lexeme "true", False <$ lexeme "false"]

number :: Parser Int
number = lexeme (L.signed empty L.decimal)

string :: Parser Text
string = lexeme ("\"" *> (T.pack <$> many (try escape <|> satisfy (/= '\"'))) <* "\"")
  where
    escape = "\\" *> choice ['"' <$ "\"", '\r' <$ "r", '\n' <$ "n", '\t' <$ "t"]

identifier :: Parser S.Identifier
identifier = lexeme (S.Identifier . T.pack <$> ((:) <$> letterChar <*> many alphaNumChar))

comma :: Parser ()
comma = () <$ lexeme ","

expression :: Parser S.Expr
expression = do
  value <-
    choice
      [ S.Array <$> try (brackets (expression `sepBy` comma)),
        S.NumLit <$> try number,
        S.StringLit <$> try string,
        S.NullLit <$ try (lexeme "null"),
        S.BoolLit <$> try boolean,
        S.Variable <$> identifier
      ]

  selection <-
    optional . try $
      lexeme dot *> do
        S.Selection value <$> selector
          <*> (fromMaybe S.Get <$> optional (try operation))

  pure (fromMaybe value selection)

dot :: Parser ()
dot = () <$ lexeme "."

dotdot :: Parser ()
dotdot = () <$ lexeme ".."

selector :: Parser S.Selector
selector = do
  s1 <- singleSelector
  fromMaybe s1 <$> (optional . try) (dot *> (S.Compose s1 <$> selector))
  where
    singleSelector =
      choice
        [ S.Field <$> try identifier,
          try (brackets (S.Slice <$> optional number <*> (dotdot *> optional number))),
          try (brackets (S.Index <$> number))
        ]

brackets :: Parser a -> Parser a
brackets = between (lexeme "[") (lexeme "]")

operation :: Parser S.Operation
operation =
  choice
    [ S.Delete <$ try (lexeme "=" <* lexeme "delete"),
      S.Set <$> (lexeme "=" *> expression),
      S.PlusEq <$> (lexeme "+=" *> expression),
      S.MinusEq <$> (lexeme "-=" *> expression),
      S.TimesEq <$> (lexeme "*=" *> expression),
      S.DivEq <$> (lexeme "/=" *> expression)
    ]
