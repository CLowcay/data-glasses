{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module DG.Runtime
  ( Function,
    Collector,
    Value (..),
    JSONF (..),
    asJSON,
    asFunction,
    toJSONM,
    fromJSONM,
    withCollector,
  )
where

import qualified Data.Aeson as J
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

type Function = [Value] -> Either String Value

type Collector a = (a, J.Value -> Either String a, a -> J.Value, a -> a -> Either String a)

data Value = JSON J.Value | forall a. Collector (Collector a) | Function Function

asJSON :: Value -> Either String J.Value
asJSON = \case JSON v -> Right v; v -> Left ("Expected JSON value, found " ++ show v)

asFunction :: Value -> Either String Function
asFunction = \case Function v -> Right v; v -> Left ("Expected function value, found " ++ show v)

withCollector :: Value -> (forall a. Collector a -> Either String b) -> Either String b
withCollector v k = case v of Collector c -> k c; _ -> Left ("Expected collector value, found " ++ show v)

instance Show Value where
  show c = case c of
    JSON v -> show v
    Function _ -> "<function>"
    Collector _ -> "<collector>"

data JSONF f
  = Null
  | Boolean Bool
  | Number Scientific
  | Str Text
  | Array (Vector (f (JSONF f)))
  | Object (HashMap Text (f (JSONF f)))

toJSONM :: J.Value -> JSONF Maybe
toJSONM = \case
  J.Object o -> Object (Just . toJSONM <$> o)
  J.Array a -> Array (Just . toJSONM <$> a)
  J.String s -> Str s
  J.Number n -> Number n
  J.Bool b -> Boolean b
  J.Null -> Null

fromJSONM :: JSONF Maybe -> J.Value
fromJSONM = \case
  Null -> J.Null
  Boolean b -> J.Bool b
  Number n -> J.Number n
  Str s -> J.String s
  Array a -> J.Array (V.mapMaybe (fmap fromJSONM) a)
  Object o -> J.Object (HM.mapMaybe (fmap fromJSONM) o)
