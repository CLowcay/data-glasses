{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DG.Syntax
  ( Identifier (..),
    Expr (..),
    Selector (..),
    Operation (..),
    Slice (..),
    ObjectElement (..),
    Unop (..),
    Binop (..),
  )
where

import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

newtype Identifier = Identifier {unIdentifier :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable)

data Expr
  = Variable Identifier
  | StringLit Text
  | NumLit Int
  | BoolLit Bool
  | NullLit
  | Array [Expr]
  | Object [ObjectElement]
  | Selection Expr (NonEmpty Selector) Operation
  | Unop Unop Expr
  | Binop Binop Expr Expr
  | Apply Expr [Expr]
  | Abstraction [Identifier] Expr
  | If Expr Expr Expr
  | Let [(Identifier, Expr)] Expr
  deriving (Eq, Show)

data ObjectElement
  = SimpleElement Text Expr
  | ExprElement Expr Expr
  | ExprAsElement Expr Identifier Expr
  deriving (Eq, Show)

data Selector
  = Field Identifier
  | Slice [Slice]
  | Map Expr
  | Where Expr
  | Collect Expr
  deriving (Eq, Show)

data Slice
  = Index Expr
  | Range (Maybe Int) (Maybe Int) (Maybe Int)
  deriving (Eq, Show)

data Operation
  = Get
  | Delete
  | Set Expr
  | SetAs Identifier Expr
  | SetAsI Identifier Identifier Expr
  | PlusEq Expr
  | MinusEq Expr
  | TimesEq Expr
  | DivEq Expr
  | ConcatEq Expr
  deriving (Eq, Show)

data Unop = Not deriving (Eq, Show)

data Binop
  = Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | Eq
  | Neq
  | Gt
  | Lt
  | Gte
  | Lte
  | And
  | Or
  | Concat
  | Union
  | Intersection
  | Difference
  deriving (Eq, Show)
