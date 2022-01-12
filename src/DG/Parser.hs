{-# LANGUAGE LambdaCase #-}
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
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1, symbolChar)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space space1 empty empty)

word :: Text -> Parser ()
word w = lexeme (try (P.string w *> notFollowedBy letterChar))

op :: Text -> Parser ()
op o = lexeme (try (P.string o *> notFollowedBy symbolChar))

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

expression :: Parser S.Expr
expression =
  choice
    [ S.If <$> (word "if" *> expression) <*> (word "then" *> expression) <*> (word "else" *> expression),
      S.Let <$> (word "let" *> (((,) <$> identifier <*> (eq *> expression)) `sepBy` semi)) <*> (inP *> expression),
      makeExprParser
        term
        [ [Prefix (S.Unop S.Not <$ op "!")],
          [ InfixL (S.Binop S.Times <$ op "*"),
            InfixL (S.Binop S.Divide <$ op "/"),
            InfixL (S.Binop S.Intersection <$ word "intersection")
          ],
          [ InfixL (S.Binop S.Plus <$ op "+"),
            InfixL (S.Binop S.Minus <$ op "-"),
            InfixL (S.Binop S.Union <$ word "union"),
            InfixL (S.Binop S.Difference <$ op "minus")
          ],
          [InfixL (S.Binop S.Modulo <$ word "mod")],
          [InfixL (S.Binop S.Concat <$ op "++")],
          [ InfixN (S.Binop S.Eq <$ op "=="),
            InfixN (S.Binop S.Neq <$ op "!="),
            InfixN (S.Binop S.Gt <$ op ">"),
            InfixN (S.Binop S.Lt <$ op "<"),
            InfixN (S.Binop S.Gte <$ op ">="),
            InfixN (S.Binop S.Lte <$ op "<=")
          ],
          [InfixL (S.Binop S.And <$ word "and")],
          [InfixL (S.Binop S.Or <$ word "or")]
        ]
    ]

term :: Parser S.Expr
term = do
  value <-
    choice
      [ parentheses expression,
        S.Abstraction <$> (as *> (identifier `sepBy` comma)) <*> (inP *> expression),
        S.Array <$> try (brackets (expression `sepBy` comma)),
        S.Object <$> braces (objectElement `sepBy` comma),
        S.StringLit <$> string,
        S.NumLit <$> number,
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
dot = () <$ op "."

comma :: Parser ()
comma = () <$ op ","

colon :: Parser ()
colon = () <$ op ":"

semi :: Parser ()
semi = () <$ op ";"

star :: Parser ()
star = () <$ op "*"

as :: Parser ()
as = () <$ word "as"

eq :: Parser ()
eq = () <$ op "="

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
        [ S.Slice <$> brackets (slice `sepBy` comma),
          dot
            *> choice
              [ S.Where <$> (whereP *> parentheses expression),
                S.Collect <$> (word "collect" *> parentheses expression),
                S.Map <$> (word "map" *> parentheses expression),
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

objectElement :: Parser S.ObjectElement
objectElement =
  S.SimpleElement <$> choice [string, S.unIdentifier <$> identifier] <*> (colon *> expression) <|> do
    key <- brackets expression
    optional (as *> identifier)
      >>= ( \case
              Nothing -> S.ExprElement key <$> (colon *> expression)
              Just keyName -> S.ExprAsElement key keyName <$> (colon *> expression)
          )

brackets :: Parser a -> Parser a
brackets = between (lexeme "[") (lexeme "]")

parentheses :: Parser a -> Parser a
parentheses = between (lexeme "(") (lexeme ")")

braces :: Parser a -> Parser a
braces = between (lexeme "{") (lexeme "}")

operation :: Parser S.Operation
operation =
  choice
    [ try (S.Delete <$ eq <* delete),
      S.Set <$> (eq *> expression),
      try (S.SetAs <$> (as *> identifier) <*> (eq *> expression)),
      S.SetAsI <$> (as *> identifier) <*> (comma *> identifier) <*> (eq *> expression),
      S.PlusEq <$> (op "+=" *> expression),
      S.MinusEq <$> (op "-=" *> expression),
      S.TimesEq <$> (op "*=" *> expression),
      S.DivEq <$> (op "/=" *> expression),
      S.ConcatEq <$> (op "++=" *> expression)
    ]
