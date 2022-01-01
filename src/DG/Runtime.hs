module DG.Runtime (Function, Value (..)) where

import qualified Data.Aeson as J

type Function = [Value] -> Either String Value

data Value = JSON J.Value | Function Function

instance Show Value where
  show c = case c of
    JSON v -> show v
    Function _ -> "<function>"
