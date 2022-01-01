{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module DG.BuiltinFunctions
  ( isNull,
    isBool,
    isString,
    isNumber,
    isObject,
    isArray,
  )
where

import Control.Monad ((>=>))
import DG.Runtime (Function, Value (..))
import qualified DG.Syntax as S
import qualified Data.Aeson as J

asJSON :: Value -> Either String J.Value
asJSON v = case v of JSON j -> Right j; _ -> Left ("Expected JSON value, found " ++ show v)

oneArg :: [Value] -> Either String Value
oneArg = \case
  [v] -> Right v
  args -> Left ("Expected 1 argument, got " ++ show (length args))

isNull :: (S.Identifier, Function)
isNull = (S.Identifier "isNull", oneArg >=> asJSON >=> (pure . JSON . J.Bool . (== J.Null)))

isBool :: (S.Identifier, Function)
isBool = (S.Identifier "isBool", oneArg >=> asJSON >=> (pure . JSON . J.Bool . (\case J.Bool _ -> True; _ -> False)))

isString :: (S.Identifier, Function)
isString = (S.Identifier "isString", oneArg >=> asJSON >=> (pure . JSON . J.Bool . (\case J.String _ -> True; _ -> False)))

isNumber :: (S.Identifier, Function)
isNumber = (S.Identifier "isNumber", oneArg >=> asJSON >=> (pure . JSON . J.Bool . (\case J.Number _ -> True; _ -> False)))

isObject :: (S.Identifier, Function)
isObject = (S.Identifier "isObject", oneArg >=> asJSON >=> (pure . JSON . J.Bool . (\case J.Object _ -> True; _ -> False)))

isArray :: (S.Identifier, Function)
isArray = (S.Identifier "isArray", oneArg >=> asJSON >=> (pure . JSON . J.Bool . (\case J.Array _ -> True; _ -> False)))
