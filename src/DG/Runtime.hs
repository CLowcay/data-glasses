{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module DG.Runtime
  ( Function (..),
    Collector,
    JsonValue,
    Value (..),
    JsonMeta (..),
    RuntimeException (..),
    runtimeError,
    asJSON,
    asFunction,
    withCollector,
  )
where

import Control.Exception (Exception, SomeException)
import Control.Monad.Catch (MonadThrow (throwM))
import DG.Json (JsonE)

data JsonMeta = Tombstone deriving (Eq, Ord, Show)

type JsonValue = JsonE JsonMeta

data Function = F Int ([Value] -> Either SomeException Value)

type Collector a = (a, JsonValue -> Either SomeException a, a -> JsonValue, a -> a -> Either SomeException a)

data Value = JSON JsonValue | forall a. Collector (Collector a) | Function Function

data RuntimeException = RuntimeException String deriving (Show)

instance Exception RuntimeException

runtimeError :: MonadThrow m => String -> m a
runtimeError = throwM . RuntimeException

asJSON :: MonadThrow m => Value -> m JsonValue
asJSON = \case JSON v -> pure v; v -> runtimeError ("Expected JSON value, found " ++ show v)

asFunction :: MonadThrow m => Value -> m Function
asFunction = \case Function v -> pure v; v -> runtimeError ("Expected function value, found " ++ show v)

withCollector :: MonadThrow m => Value -> (forall a. Collector a -> m b) -> m b
withCollector v k = case v of Collector c -> k c; _ -> runtimeError ("Expected collector value, found " ++ show v)

instance Show Value where
  show c = case c of
    JSON v -> show v
    Function (F arity _) -> "<function:" ++ show arity ++ ">"
    Collector _ -> "<collector>"
