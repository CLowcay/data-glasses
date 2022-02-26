{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module DG.Parser (expression, number) where

import Control.Applicative (optional, (<|>))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified DG.Syntax as DG
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty, some1)
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

identifier :: Parser DG.Identifier
identifier = lexeme (DG.Identifier . T.pack <$> ((:) <$> letterChar <*> many alphaNumChar))

expression :: Parser DG.Expr
expression =
  choice
    [ DG.If <$> (word "if" *> expression) <*> (word "then" *> expression) <*> (word "else" *> expression),
      DG.Let <$> (word "let" *> (((,) <$> identifier <*> (eq *> expression)) `sepBy` semi)) <*> (inP *> expression),
      makeExprParser
        term
        [ [Prefix (DG.Unop DG.Not <$ op "!")],
          [ InfixL (DG.Binop DG.Times <$ op "*"),
            InfixL (DG.Binop DG.Divide <$ op "/"),
            InfixL (DG.Binop DG.Intersection <$ word "intersection")
          ],
          [ InfixL (DG.Binop DG.Plus <$ op "+"),
            InfixL (DG.Binop DG.Minus <$ op "-"),
            InfixL (DG.Binop DG.Union <$ word "union"),
            InfixL (DG.Binop DG.Difference <$ op "minus")
          ],
          [InfixL (DG.Binop DG.Modulo <$ word "mod")],
          [InfixL (DG.Binop DG.Concat <$ op "++")],
          [ InfixN (DG.Binop DG.Eq <$ op "=="),
            InfixN (DG.Binop DG.Neq <$ op "!="),
            InfixN (DG.Binop DG.Gt <$ op ">"),
            InfixN (DG.Binop DG.Lt <$ op "<"),
            InfixN (DG.Binop DG.Gte <$ op ">="),
            InfixN (DG.Binop DG.Lte <$ op "<=")
          ],
          [InfixL (DG.Binop DG.And <$ word "and")],
          [InfixL (DG.Binop DG.Or <$ word "or")]
        ]
    ]

term :: Parser DG.Expr
term = do
  value <-
    choice
      [ parentheses expression,
        DG.Abstraction <$> (as *> (identifier `sepBy` comma)) <*> (inP *> expression),
        DG.Array <$> try (brackets (expression `sepBy` comma)),
        DG.Object <$> braces (objectElement `sepBy` comma),
        DG.StringLit <$> string,
        DG.NumLit <$> number,
        DG.NullLit <$ try (word "null"),
        DG.BoolLit <$> try boolean,
        DG.Variable <$> identifier
      ]

  selection <-
    optional $ do
      DG.Selection value <$> selector
        <*> (fromMaybe DG.Get <$> optional operation)

  applications <- many (parentheses (expression `sepBy` comma))
  pure (foldl' DG.Apply (fromMaybe value selection) applications)

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

selector :: Parser (NonEmpty DG.Selector)
selector = some1 singleSelector
  where
    singleSelector =
      choice
        [ DG.Slice <$> brackets (slice `sepBy` comma),
          dot
            *> choice
              [ DG.Where <$> (whereP *> parentheses expression),
                DG.Collect <$> (word "collect" *> parentheses expression),
                DG.Map <$> (word "map" *> parentheses expression),
                DG.Field <$> identifier
              ]
        ]

slice :: Parser DG.Slice
slice =
  choice
    [ DG.Range Nothing Nothing Nothing <$ star,
      DG.Range Nothing <$> (Just <$> (colon *> number)) <*> optional (colon *> number),
      try (DG.Range <$> (Just <$> number) <*> (colon *> optional number) <*> optional (colon *> number)),
      DG.Index <$> expression
    ]

objectElement :: Parser DG.ObjectElement
objectElement =
  DG.SimpleElement <$> choice [string, DG.unIdentifier <$> identifier] <*> (colon *> expression) <|> do
    key <- brackets expression
    optional (as *> identifier)
      >>= ( \case
              Nothing -> DG.ExprElement key <$> (colon *> expression)
              Just keyName -> DG.ExprAsElement key keyName <$> (colon *> expression)
          )

brackets :: Parser a -> Parser a
brackets = between (lexeme "[") (lexeme "]")

parentheses :: Parser a -> Parser a
parentheses = between (lexeme "(") (lexeme ")")

braces :: Parser a -> Parser a
braces = between (lexeme "{") (lexeme "}")

operation :: Parser DG.Operation
operation =
  choice
    [ try (DG.Delete <$ eq <* delete),
      DG.Set <$> (eq *> expression),
      try (DG.SetAs <$> (as *> identifier) <*> (eq *> expression)),
      DG.SetAsI <$> (as *> identifier) <*> (comma *> identifier) <*> (eq *> expression),
      DG.PlusEq <$> (op "+=" *> expression),
      DG.MinusEq <$> (op "-=" *> expression),
      DG.TimesEq <$> (op "*=" *> expression),
      DG.DivEq <$> (op "/=" *> expression),
      DG.ConcatEq <$> (op "++=" *> expression)
    ]
