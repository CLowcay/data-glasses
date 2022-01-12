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
    joinCollector,
    stringify,
    concatCollector,
    unionCollector,
    intersectionCollector,
  )
where

import Control.Monad ((>=>))
import DG.Runtime (Collector, Function (F), Value (..))
import qualified DG.Syntax as S
import qualified Data.Aeson as J
import qualified Data.Aeson.Text as J
import Data.Foldable (toList)
import Data.Functor ((<&>))
import qualified Data.HashMap.Lazy as HM
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Vector (Vector)
import qualified Data.Vector as V

asJSON :: Value -> Either String J.Value
asJSON v = case v of JSON j -> Right j; _ -> Left ("Expected JSON value, found " ++ show v)

oneArg :: [Value] -> Either String Value
oneArg = \case
  [v] -> Right v
  args -> Left ("Expected 1 argument, found " ++ show (length args))

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

stringify :: (S.Identifier, Function)
stringify = (S.Identifier "stringify", F 1 $ oneArg >=> asJSON >=> pure . JSON . \case J.String s -> J.String s; v -> J.String (LT.toStrict (J.encodeToLazyText v)))

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

unionCollector :: (S.Identifier, Collector J.Object)
unionCollector = (S.Identifier "unionAll", (HM.empty, asObject, J.Object, \a b -> pure (a `HM.union` b)))

intersectionCollector :: (S.Identifier, Collector J.Object)
intersectionCollector = (S.Identifier "intersectionAll", (HM.empty, asObject, J.Object, \a b -> pure (a `HM.intersection` b)))

concatCollector :: (S.Identifier, Collector [J.Value])
concatCollector = (S.Identifier "concat", ([], fmap toList . asArray, J.Array . V.fromList, \a b -> pure (a <> b)))

joinCollector :: (S.Identifier, Function)
joinCollector =
  ( S.Identifier "join",
    F 1 $
      oneArg >=> asJSON >=> asString >=> \seperator ->
        Right
          ( Collector
              ( "",
                fmap (<> seperator) . asString,
                J.String . T.dropEnd (T.length seperator),
                \a b -> pure (a <> b)
              )
          )
  )

asNumber :: J.Value -> Either String Scientific
asNumber = \case J.Number n -> pure n; x -> Left ("Expected number, found " ++ show x)

asObject :: J.Value -> Either String J.Object
asObject = \case J.Object o -> pure o; x -> Left ("Expected object, found " ++ show x)

asArray :: J.Value -> Either String (Vector J.Value)
asArray = \case J.Array a -> pure a; x -> Left ("Expected array, found " ++ show x)

asString :: J.Value -> Either String Text
asString = \case J.String s -> pure s; x -> Left ("Expected string, found " ++ show x)
