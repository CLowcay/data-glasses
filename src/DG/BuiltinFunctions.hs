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
    concatCollector,
    stringify,
    unionCollector,
    intersectionCollector,
  )
where

import Control.Exception (SomeException)
import Control.Monad ((>=>))
import DG.Json (JsonE (..), Object, compactPrint, deannotate)
import DG.Runtime (Collector, Function (F), JsonMeta, JsonValue, Value (..), runtimeError)
import qualified DG.Syntax as DG
import qualified Data.ByteString.Builder as BB
import Data.Foldable (toList)
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Vector (Vector)
import qualified Data.Vector as V

asJSON :: Value -> Either SomeException JsonValue
asJSON v = case v of JSON j -> Right j; _ -> runtimeError ("Expected JSON value, found " ++ show v)

oneArg :: [Value] -> Either SomeException Value
oneArg = \case
  [v] -> Right v
  args -> runtimeError ("Expected 1 argument, found " ++ show (length args))

isNull :: (DG.Identifier, Function)
isNull = (DG.Identifier "isNull", F 1 $ oneArg >=> asJSON >=> pure . JSON . Bool . (== Null))

isBool :: (DG.Identifier, Function)
isBool = (DG.Identifier "isBool", F 1 $ oneArg >=> asJSON >=> pure . JSON . Bool . (\case Bool _ -> True; _ -> False))

isString :: (DG.Identifier, Function)
isString = (DG.Identifier "isString", F 1 $ oneArg >=> asJSON >=> pure . JSON . Bool . (\case String _ -> True; _ -> False))

isNumber :: (DG.Identifier, Function)
isNumber = (DG.Identifier "isNumber", F 1 $ oneArg >=> asJSON >=> pure . JSON . Bool . (\case Number _ -> True; _ -> False))

isObject :: (DG.Identifier, Function)
isObject = (DG.Identifier "isObject", F 1 $ oneArg >=> asJSON >=> pure . JSON . Bool . (\case Object _ -> True; _ -> False))

isArray :: (DG.Identifier, Function)
isArray = (DG.Identifier "isArray", F 1 $ oneArg >=> asJSON >=> pure . JSON . Bool . (\case Array _ -> True; _ -> False))

stringify :: (DG.Identifier, Function)
stringify =
  ( DG.Identifier "stringify",
    F 1 $ oneArg >=> asJSON >=> pure . JSON . \case String s -> String s; v -> String (encodeJson v)
  )
  where
    encodeJson v = maybe "" (encodeToText . compactPrint) (deannotate v)
    encodeToText = LT.toStrict . LT.decodeUtf8With T.lenientDecode . BB.toLazyByteString

sumCollector :: (DG.Identifier, Collector Scientific)
sumCollector = (DG.Identifier "sum", (0, asNumber, Number, \a b -> pure (a + b)))

productCollector :: (DG.Identifier, Collector Scientific)
productCollector = (DG.Identifier "product", (0, asNumber, Number, \a b -> pure (a * b)))

countCollector :: (DG.Identifier, Collector Scientific)
countCollector = (DG.Identifier "count", (0, const (pure 1), Number, \a b -> pure (a + b)))

meanCollector :: (DG.Identifier, Collector (Scientific, Int))
meanCollector =
  ( DG.Identifier "mean",
    ( (0, 0),
      \v -> asNumber v <&> (,1),
      \(t, n) -> Number (fromFloatDigits (toRealFloat t / (fromIntegral n :: Double))),
      \(t1, n1) (t2, n2) -> pure (t1 + t2, n1 + n2)
    )
  )

asArrayCollector :: (DG.Identifier, Collector [JsonValue])
asArrayCollector = (DG.Identifier "asArray", ([], pure . pure, Array . V.fromList, \a b -> pure (a ++ b)))

unionCollector :: (DG.Identifier, Collector (Object JsonMeta))
unionCollector = (DG.Identifier "unionAll", (M.empty, asObject, Object, \a b -> pure (a `M.union` b)))

intersectionCollector :: (DG.Identifier, Collector (Object JsonMeta))
intersectionCollector = (DG.Identifier "intersectionAll", (M.empty, asObject, Object, \a b -> pure (a `M.intersection` b)))

concatCollector :: (DG.Identifier, Collector [JsonValue])
concatCollector = (DG.Identifier "concat", ([], fmap toList . asArray, Array . V.fromList, \a b -> pure (a <> b)))

joinCollector :: (DG.Identifier, Function)
joinCollector =
  ( DG.Identifier "join",
    F 1 $
      oneArg >=> asJSON >=> asString >=> \seperator ->
        Right
          ( Collector
              ( "",
                fmap (<> seperator) . asString,
                String . T.dropEnd (T.length seperator),
                \a b -> pure (a <> b)
              )
          )
  )

asNumber :: JsonValue -> Either SomeException Scientific
asNumber = \case Number n -> pure n; x -> runtimeError ("Expected number, found " ++ show x)

asObject :: JsonValue -> Either SomeException (Object JsonMeta)
asObject = \case Object o -> pure o; x -> runtimeError ("Expected object, found " ++ show x)

asArray :: JsonValue -> Either SomeException (Vector JsonValue)
asArray = \case Array a -> pure a; x -> runtimeError ("Expected array, found " ++ show x)

asString :: JsonValue -> Either SomeException Text
asString = \case String s -> pure s; x -> runtimeError ("Expected string, found " ++ show x)
