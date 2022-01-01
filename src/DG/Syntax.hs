{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DG.Syntax where

import Data.Hashable (Hashable)
import Data.Text (Text)

newtype Identifier = Identifier Text
  deriving stock (Eq, Show)
  deriving newtype (Hashable)

data Expr
  = Variable Identifier
  | StringLit Text
  | NumLit Int
  | BoolLit Bool
  | NullLit
  | Array [Expr]
  | Selection Expr Selector Operation
  deriving (Eq, Show)

data Selector
  = Field Identifier
  | Slice Slice
  | Compose Selector Selector
  deriving (Eq, Show)

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
  deriving (Eq, Show)
