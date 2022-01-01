{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module DG.Interpreter (Function, Value (..), evaluate) where

import Control.Monad (join)
import Control.Monad.Identity (runIdentity)
import qualified DG.Syntax as S
import qualified Data.Aeson as J
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector ((!), (//))
import qualified Data.Vector as V

type Function = [Value] -> Value

data Value = JSON J.Value | Function Function

instance Show Value where
  show c = case c of
    JSON v -> show v
    Function f -> "<function>"

type Context = HashMap S.Identifier Value

lookupCtx :: Context -> S.Identifier -> Either String Value
lookupCtx ctx var = maybe (Left ("Undefined variable " ++ show var)) Right (ctx !? var)

lookupValue :: Context -> S.Identifier -> Either String J.Value
lookupValue ctx var = lookupCtx ctx var >>= \case JSON v -> pure v; v -> Left ("Expected JSON value for " ++ show var ++ " but found " ++ show v)

lookupFunction :: Context -> S.Identifier -> Either String Function
lookupFunction ctx var = lookupCtx ctx var >>= \case Function v -> pure v; v -> Left (show var ++ " is not a function")

get :: S.Selector -> J.Value -> [J.Value]
get s v = case s of
  S.Field (S.Identifier name) -> case v of
    J.Object o -> toList (o !? name)
    _ -> []
  S.Slice slice -> case v of
    J.Array a ->
      case slice of
        S.Index i -> [a ! i | i >= 0 && i < V.length a]
        S.Range from to _ ->
          let efrom = fromMaybe 0 from
              rlen = fromMaybe (V.length a) to - efrom
           in toList (V.slice efrom rlen a)
    _ -> []
  S.Compose s1 s2 -> get s1 v >>= get s2

delete :: S.Selector -> J.Value -> J.Value
delete s v = case s of
  S.Field (S.Identifier name) -> case v of
    J.Object o -> J.Object (HM.delete name o)
    _ -> v
  S.Slice slice -> case v of
    J.Array a ->
      case slice of
        S.Index i -> J.Array (let (a1, rs) = V.splitAt i a in a1 <> V.drop 1 rs)
        S.Range from to _ ->
          let efrom = fromMaybe 0 from
              rlen = fromMaybe (V.length a) to - efrom
              (hs, as) = V.splitAt efrom a
              (_, ts) = V.splitAt rlen as
           in J.Array (hs <> ts)
    _ -> v
  S.Compose s1 s2 -> runIdentity (tmap s1 (pure . delete s2) v)

tmap :: Monad t => S.Selector -> (J.Value -> t J.Value) -> J.Value -> t J.Value
tmap s f v = case s of
  S.Field (S.Identifier name) -> case v of
    J.Object o -> case o !? name of
      Nothing -> pure (J.Object o)
      Just v -> f v <&> \v' -> J.Object (HM.insert name v' o)
    _ -> pure v
  S.Slice slice -> case v of
    J.Array a ->
      case slice of
        S.Index i ->
          if i >= 0 && i < V.length a
            then f (a ! i) <&> \v -> J.Array (a // [(i, v)])
            else pure v
        S.Range from to _ ->
          let inRange i = maybe True (i >=) from && maybe True (i <) to
           in J.Array <$> V.imapM (\i x -> if inRange i then f x else pure x) a
    _ -> pure v
  S.Compose s1 s2 -> tmap s1 (tmap s2 f) v

evaluate :: Context -> S.Expr -> Either String [J.Value]
evaluate ctx e = case e of
  S.Variable var -> pure <$> lookupValue ctx var
  S.StringLit text -> Right [J.String text]
  S.NumLit n -> Right [J.Number (fromIntegral n)]
  S.NullLit -> Right [J.Null]
  S.BoolLit b -> Right [J.Bool b]
  S.Array as -> pure . J.Array . V.fromList . join <$> traverse (evaluate ctx) as
  S.Binop op lExpr rExpr -> do
    l <- asSingle =<< evaluate ctx lExpr
    r <- asSingle =<< evaluate ctx rExpr
    pure <$> applyOp op l r
  S.Selection expr selector operation -> do
    v <- evaluate ctx expr
    case operation of
      S.Get -> Right (get selector =<< v)
      S.Delete -> Right (delete selector <$> v)
      S.Set ex ->
        evaluate ctx ex >>= asSingle >>= \x -> traverse (tmap selector (Right . const x)) v
      S.SetAs var ex -> traverse (tmap selector (\x -> evaluate (HM.insert var (JSON x) ctx) ex >>= asSingle)) v
      S.PlusEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> traverse (tmap selector (Right . numOp (+ x))) v
      S.MinusEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> traverse (tmap selector (Right . numOp (subtract x))) v
      S.TimesEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> traverse (tmap selector (Right . numOp (* x))) v
      S.DivEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> traverse (tmap selector (Right . numOp (/ x))) v
      S.ConcatEq ex ->
        evaluate ctx ex >>= asSingle >>= asString >>= \x -> traverse (tmap selector (Right . stringOp (<> x))) v

asSingle :: [J.Value] -> Either String J.Value
asSingle [x] = Right x
asSingle xs = Left ("Expected one result, found " ++ show xs)

asNumber :: J.Value -> Either String Scientific
asNumber v = case v of
  J.Number n -> Right n
  _ -> Left ("Expected a number, found " ++ show v)

asBool :: J.Value -> Either String Bool
asBool v = case v of
  J.Bool n -> Right n
  _ -> Left ("Expected true or false, found " ++ show v)

asString :: J.Value -> Either String Text
asString v = case v of
  J.String n -> Right n
  _ -> Left ("Expected a string, found " ++ show v)

numOp :: (Scientific -> Scientific) -> J.Value -> J.Value
numOp f = \case
  J.Number n -> J.Number (f n)
  v -> v

stringOp :: (Text -> Text) -> J.Value -> J.Value
stringOp f = \case
  J.String n -> J.String (f n)
  v -> v

numBinop :: (Scientific -> Scientific -> Scientific) -> J.Value -> J.Value -> Either String J.Value
numBinop f l r = J.Number <$> (f <$> asNumber l <*> asNumber r)

boolBinop :: (Bool -> Bool -> Bool) -> J.Value -> J.Value -> Either String J.Value
boolBinop f l r = J.Bool <$> (f <$> asBool l <*> asBool r)

stringBinop :: (Text -> Text -> Text) -> J.Value -> J.Value -> Either String J.Value
stringBinop f l r = J.String <$> (f <$> asString l <*> asString r)

applyOp :: S.Binop -> J.Value -> J.Value -> Either String J.Value
applyOp op l r = case op of
  S.Plus -> numBinop (+) l r
  S.Minus -> numBinop (-) l r
  S.Times -> numBinop (*) l r
  S.Divide -> numBinop (/) l r
  S.Eq -> pure (J.Bool (l == r))
  S.Neq -> pure (J.Bool (l /= r))
  S.Gt -> pure (J.Bool (l > r))
  S.Lt -> pure (J.Bool (l < r))
  S.Gte -> pure (J.Bool (l >= r))
  S.Lte -> pure (J.Bool (l <= r))
  S.And -> boolBinop (&&) l r
  S.Or -> boolBinop (||) l r
  S.Concat -> stringBinop (<>) l r
