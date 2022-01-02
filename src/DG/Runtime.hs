{-# LANGUAGE LambdaCase #-}

module DG.Runtime (Function, Value (..), asJSON, asFunction) where

import qualified Data.Aeson as J

type Function = [Value] -> Either String Value

data Value = JSON J.Value | Function Function

asJSON :: Value -> Either String J.Value
asJSON = \case JSON v -> Right v; v -> Left ("Expected JSON value, found" ++ show v)

asFunction :: Value -> Either String Function
asFunction = \case Function v -> Right v; v -> Left ("Expected function value, found" ++ show v)

instance Show Value where
  show c = case c of
    JSON v -> show v
    Function _ -> "<function>"
