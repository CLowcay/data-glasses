{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module DG.Runtime
  ( Function,
    Value (..),
    JSONF (..),
    asJSON,
    asFunction,
    toJSONM,
    fromJSONM,
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

data Value = JSON J.Value | Function Function

asJSON :: Value -> Either String J.Value
asJSON = \case JSON v -> Right v; v -> Left ("Expected JSON value, found" ++ show v)

asFunction :: Value -> Either String Function
asFunction = \case Function v -> Right v; v -> Left ("Expected function value, found" ++ show v)

instance Show Value where
  show c = case c of
    JSON v -> show v
    Function _ -> "<function>"

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
