{-# LANGUAGE OverloadedStrings #-}

module DG.Parser (expression, number) where

import Control.Applicative (optional, (<|>))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified DG.Syntax as S
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (notFollowedBy, try), Parsec, between, choice, empty, many, satisfy, sepBy)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space space1 empty empty)

word :: Text -> Parser ()
word w = lexeme (P.string w *> notFollowedBy letterChar)

boolean :: Parser Bool
boolean = choice [True <$ word "true", False <$ word "false"]

number :: Parser Int
number = lexeme (L.signed (() <$ "") L.decimal)

string :: Parser Text
string = lexeme ("\"" *> (T.pack <$> many (try escape <|> satisfy (/= '\"'))) <* "\"")
  where
    escape = "\\" *> choice ['"' <$ "\"", '\r' <$ "r", '\n' <$ "n", '\t' <$ "t"]

identifier :: Parser S.Identifier
identifier = lexeme (S.Identifier . T.pack <$> ((:) <$> letterChar <*> many alphaNumChar))

comma :: Parser ()
comma = () <$ lexeme ","

expression :: Parser S.Expr
expression =
  choice
    [ S.If <$> (word "if" *> expression) <*> (word "then" *> expression) <*> (word "else" *> expression),
      makeExprParser
        term
        [ [Prefix (S.Unop S.Not <$ lexeme "!")],
          [InfixL (S.Binop S.Times <$ lexeme "*"), InfixL (S.Binop S.Divide <$ lexeme "/")],
          [InfixL (S.Binop S.Plus <$ lexeme "+"), InfixL (S.Binop S.Minus <$ lexeme "-")],
          [InfixL (S.Binop S.Modulo <$ word "mod")],
          [InfixL (S.Binop S.Concat <$ lexeme "++")],
          [ InfixN (S.Binop S.Eq <$ lexeme "=="),
            InfixN (S.Binop S.Neq <$ lexeme "!="),
            InfixN (S.Binop S.Gt <$ lexeme ">"),
            InfixN (S.Binop S.Lt <$ lexeme "<"),
            InfixN (S.Binop S.Gte <$ lexeme ">="),
            InfixN (S.Binop S.Lte <$ lexeme "<=")
          ],
          [InfixL (S.Binop S.And <$ word "and")],
          [InfixL (S.Binop S.Or <$ word "or")],
          [InfixL (S.Sequence <$ comma)]
        ]
    ]

term :: Parser S.Expr
term = do
  value <-
    choice
      [ parentheses expression,
        S.Abstraction <$> (as *> (identifier `sepBy` comma)) <*> (inP *> expression),
        S.Array <$> try (brackets (expression `sepBy` comma)),
        S.NumLit <$> try number,
        S.StringLit <$> try string,
        S.NullLit <$ try (word "null"),
        S.BoolLit <$> try boolean,
        S.Variable <$> identifier
      ]

  selection <-
    optional $ do
      S.Selection value <$> selector
        <*> (fromMaybe S.Get <$> optional operation)

  applications <- many (parentheses (expression `sepBy` comma))
  pure (foldl' S.Apply (fromMaybe value selection) applications)

dot :: Parser ()
dot = () <$ lexeme "."

colon :: Parser ()
colon = () <$ lexeme ":"

star :: Parser ()
star = () <$ lexeme "*"

as :: Parser ()
as = () <$ word "as"

eq :: Parser ()
eq = () <$ lexeme "="

delete :: Parser ()
delete = () <$ word "delete"

whereP :: Parser ()
whereP = () <$ word "where"

inP :: Parser ()
inP = () <$ word "in"

selector :: Parser S.Selector
selector = do
  s1 <- singleSelector
  fromMaybe s1 <$> (optional . try) (S.Compose s1 <$> selector)
  where
    singleSelector =
      choice
        [ S.Slice <$> brackets slice,
          dot
            *> choice
              [ S.Where <$> (whereP *> parentheses expression),
                S.Collect <$> (word "collect" *> parentheses expression),
                S.Field <$> identifier
              ]
        ]

slice :: Parser S.Slice
slice =
  choice
    [ S.Range Nothing Nothing Nothing <$ star,
      S.Range Nothing <$> (Just <$> (colon *> number)) <*> optional (colon *> number),
      try (S.Range <$> (Just <$> number) <*> (colon *> optional number) <*> optional (colon *> number)),
      S.Index <$> expression
    ]

brackets :: Parser a -> Parser a
brackets = between (lexeme "[") (lexeme "]")

parentheses :: Parser a -> Parser a
parentheses = between (lexeme "(") (lexeme ")")

operation :: Parser S.Operation
operation =
  choice
    [ S.Delete <$ try (eq <* delete),
      S.Set <$> (eq *> expression),
      S.SetAs <$> (as *> identifier <* eq) <*> expression,
      S.PlusEq <$> (lexeme "+=" *> expression),
      S.MinusEq <$> (lexeme "-=" *> expression),
      S.TimesEq <$> (lexeme "*=" *> expression),
      S.DivEq <$> (lexeme "/=" *> expression),
      S.ConcatEq <$> (lexeme "++=" *> expression)
    ]
