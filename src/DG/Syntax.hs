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
  | Selection Expr Selector Operation
  deriving (Eq, Show)

data Selector
  = Field Identifier
  | Index Int
  | Slice (Maybe Int) (Maybe Int)
  | Compose Selector Selector
  deriving (Eq, Show)

data Operation
  = Get
  | Set Expr
  | PlusEq Expr
  | MinusEq Expr
  | TimesEq Expr
  | DivEq Expr
  deriving (Eq, Show)
