{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DG.Syntax
  ( Identifier (..),
    Expr (..),
    Selector (..),
    Operation (..),
    Slice (..),
    Filter (..),
    Unop (..),
    Binop (..),
  )
where

import Data.Hashable (Hashable)
import Data.Text (Text)

newtype Identifier = Identifier Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable)

data Expr
  = Variable Identifier
  | StringLit Text
  | NumLit Int
  | BoolLit Bool
  | NullLit
  | Array [Expr]
  | Selection Expr Selector Operation
  | Unop Unop Expr
  | Binop Binop Expr Expr
  | Apply Expr [Expr]
  | Abstraction [Identifier] Expr
  deriving (Eq, Show)

data Selector
  = Field Identifier
  | Slice Slice (Maybe Filter)
  | Compose Selector Selector
  deriving (Eq, Show)

data Filter = Filter Identifier Expr deriving (Eq, Show)

data Slice
  = Index Int
  | Range (Maybe Int) (Maybe Int) (Maybe Int)
  deriving (Eq, Show)

data Operation
  = Get
  | Delete
  | Set Expr
  | SetAs Identifier Expr
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
  | Eq
  | Neq
  | Gt
  | Lt
  | Gte
  | Lte
  | And
  | Or
  | Concat
  deriving (Eq, Show)
