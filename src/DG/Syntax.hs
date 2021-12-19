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

data Selector = Field Identifier deriving (Eq, Show)

data Operation = Get | Set Expr deriving (Eq, Show)
