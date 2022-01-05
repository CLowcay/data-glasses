{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DG.BuiltinFunctions
  ( isNull,
    isBool,
    isString,
    isNumber,
    isObject,
    isArray,
    sumCollector,
    productCollector,
    countCollector,
    asArrayCollector,
    meanCollector,
  )
where

import Control.Monad ((>=>))
import DG.Runtime (Collector, Function (F), Value (..))
import qualified DG.Syntax as S
import qualified Data.Aeson as J
import Data.Functor ((<&>))
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import qualified Data.Vector as V

asJSON :: Value -> Either String J.Value
asJSON v = case v of JSON j -> Right j; _ -> Left ("Expected JSON value, found " ++ show v)

oneArg :: [Value] -> Either String Value
oneArg = \case
  [v] -> Right v
  args -> Left ("Expected 1 argument, got " ++ show (length args))

isNull :: (S.Identifier, Function)
isNull = (S.Identifier "isNull", F 1 $ oneArg >=> asJSON >=> pure . JSON . J.Bool . (== J.Null))

isBool :: (S.Identifier, Function)
isBool = (S.Identifier "isBool", F 1 $ oneArg >=> asJSON >=> pure . JSON . J.Bool . (\case J.Bool _ -> True; _ -> False))

isString :: (S.Identifier, Function)
isString = (S.Identifier "isString", F 1 $ oneArg >=> asJSON >=> pure . JSON . J.Bool . (\case J.String _ -> True; _ -> False))

isNumber :: (S.Identifier, Function)
isNumber = (S.Identifier "isNumber", F 1 $ oneArg >=> asJSON >=> pure . JSON . J.Bool . (\case J.Number _ -> True; _ -> False))

isObject :: (S.Identifier, Function)
isObject = (S.Identifier "isObject", F 1 $ oneArg >=> asJSON >=> pure . JSON . J.Bool . (\case J.Object _ -> True; _ -> False))

isArray :: (S.Identifier, Function)
isArray = (S.Identifier "isArray", F 1 $ oneArg >=> asJSON >=> pure . JSON . J.Bool . (\case J.Array _ -> True; _ -> False))

sumCollector :: (S.Identifier, Collector Scientific)
sumCollector = (S.Identifier "sum", (0, asNumber, J.Number, \a b -> pure (a + b)))

productCollector :: (S.Identifier, Collector Scientific)
productCollector = (S.Identifier "product", (0, asNumber, J.Number, \a b -> pure (a * b)))

countCollector :: (S.Identifier, Collector Scientific)
countCollector = (S.Identifier "count", (0, const (pure 1), J.Number, \a b -> pure (a + b)))

meanCollector :: (S.Identifier, Collector (Scientific, Int))
meanCollector =
  ( S.Identifier "mean",
    ( (0, 0),
      \v -> asNumber v <&> (,1),
      \(t, n) -> J.Number (fromFloatDigits (toRealFloat t / (fromIntegral n :: Double))),
      \(t1, n1) (t2, n2) -> pure (t1 + t2, n1 + n2)
    )
  )

asArrayCollector :: (S.Identifier, Collector [J.Value])
asArrayCollector = (S.Identifier "asArray", ([], pure . pure, J.Array . V.fromList, \a b -> pure (a ++ b)))

asNumber :: J.Value -> Either String Scientific
asNumber = \case J.Number n -> pure n; x -> Left ("Expected number, found " ++ show x)
