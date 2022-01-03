{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module DG.Interpreter (evaluate, initialContext) where

import Control.Monad (join, (<=<))
import DG.BuiltinFunctions (isArray, isBool, isNull, isNumber, isObject, isString)
import DG.Runtime (JSONF (..), Value (..), asFunction, asJSON, fromJSONM, toJSONM)
import qualified DG.Syntax as S
import qualified Data.Aeson as J
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.HashMap.Lazy ((!?))
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector ((!), (//))
import qualified Data.Vector as V

type Context = Map S.Identifier Value

initialContext :: Context
initialContext =
  M.fromList
    ( second Function
        <$> [isNull, isBool, isString, isNumber, isObject, isArray]
    )

lookupCtx :: Context -> S.Identifier -> Either String Value
lookupCtx ctx var = maybe (Left ("Undefined variable " ++ show var)) Right (M.lookup var ctx)

get :: Context -> S.Selector -> J.Value -> Either String [J.Value]
get ctx s v = case s of
  S.Field (S.Identifier name) -> case v of
    J.Object o -> Right (toList (o !? name))
    _ -> pure []
  S.Slice slice -> case v of
    J.Array a ->
      case slice of
        S.Index i -> Right [a ! i | i >= 0 && i < V.length a]
        S.Range from to _ ->
          let efrom = fromMaybe 0 from
              rlen = fromMaybe (V.length a) to - efrom
           in Right (V.toList (V.slice efrom rlen a))
    _ -> pure []
  S.Where fexpr -> evaluateFilter ctx fexpr v <&> \passed -> [v | passed]
  S.Compose s1 s2 -> do
    r1s <- get ctx s1 v
    concat <$> traverse (get ctx s2) r1s

tmap :: Context -> S.Selector -> (J.Value -> Either String (J.Value)) -> [J.Value] -> Either String [Value]
tmap ctx s f v = mapMaybe (fmap (JSON . fromJSONM)) <$> traverse (tmapM ctx s (fmap (Just . toJSONM) . f . fromJSONM)) (toJSONM <$> v)

tmapM ::
  Context ->
  S.Selector ->
  (JSONF Maybe -> Either String (Maybe (JSONF Maybe))) ->
  JSONF Maybe ->
  Either String (Maybe (JSONF Maybe))
tmapM ctx s f v = case s of
  S.Field (S.Identifier name) ->
    Just <$> case v of
      Object o -> case o !? name of
        Just (Just vk) -> f vk <&> \v' -> Object (HM.insert name v' o)
        _ -> pure (Object o)
      _ -> pure v
  S.Slice slice ->
    Just <$> case v of
      Array a ->
        case slice of
          S.Index i ->
            if i >= 0 && i < V.length a
              then (\case Nothing -> pure Nothing; Just x -> f x) (a ! i) <&> \vi -> Array (a // [(i, vi)])
              else pure v
          S.Range from to _ ->
            let inRange i = maybe True (i >=) from && maybe True (i <) to
                mapElement i = \case Just x -> if inRange i then f x else pure (Just x); Nothing -> pure Nothing
             in Array <$> V.imapM mapElement a
      _ -> pure v
  S.Where fexpr ->
    evaluateFilter ctx fexpr (fromJSONM v)
      >>= \passed -> if passed then f v else pure (Just v)
  S.Compose s1 s2 -> tmapM ctx s1 (tmapM ctx s2 f) v

evaluate :: Context -> S.Expr -> Either String [Value]
evaluate ctx e = case e of
  S.Variable var -> pure <$> lookupCtx ctx var
  S.StringLit text -> Right [JSON (J.String text)]
  S.NumLit n -> Right [JSON (J.Number (fromIntegral n))]
  S.NullLit -> Right [JSON J.Null]
  S.BoolLit b -> Right [JSON (J.Bool b)]
  S.Array as -> pure . JSON . J.Array . V.fromList . join <$> traverse (traverse asJSON <=< evaluate ctx) as
  S.Binop op lExpr rExpr -> do
    l <- asSingle =<< evaluate ctx lExpr
    r <- asSingle =<< evaluate ctx rExpr
    pure <$> applyOp op l r
  S.Unop op expr -> pure <$> (evaluate ctx expr >>= asSingle >>= applyUnop op)
  S.Apply fExpr pExprs -> do
    f <- asFunction =<< asSingle =<< evaluate ctx fExpr
    parameters <- traverse (asSingle <=< evaluate ctx) pExprs
    pure <$> f parameters
  S.Abstraction params expr -> Right . pure . Function $ \args ->
    if length args /= length params
      then Left ("Expected " ++ show (length params) ++ " arguments, found " ++ show (length args))
      else asSingle =<< evaluate (M.fromList (params `zip` args) `M.union` ctx) expr
  S.Selection expr selector operation -> do
    v <- traverse asJSON =<< evaluate ctx expr
    case operation of
      S.Get -> fmap JSON . concat <$> traverse (get ctx selector) v
      S.Delete ->
        mapMaybe (fmap (JSON . fromJSONM)) <$> traverse (tmapM ctx selector (const (Right Nothing))) (toJSONM <$> v)
      S.Set ex ->
        evaluate ctx ex >>= asSingle >>= asJSON >>= \x -> tmap ctx selector (Right . const x) v
      S.SetAs var ex ->
        tmap ctx selector (\x -> evaluate (M.insert var (JSON x) ctx) ex >>= asSingle >>= asJSON) v
      S.PlusEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> (tmap ctx selector (Right . numOp (+ x))) v
      S.MinusEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> (tmap ctx selector (Right . numOp (subtract x))) v
      S.TimesEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> (tmap ctx selector (Right . numOp (* x))) v
      S.DivEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> (tmap ctx selector (Right . numOp (/ x))) v
      S.ConcatEq ex ->
        evaluate ctx ex >>= asSingle >>= asString >>= \x -> (tmap ctx selector (Right . stringOp (<> x))) v

evaluateFilter :: Context -> S.Expr -> J.Value -> Either String Bool
evaluateFilter ctx expr v = evaluate ctx expr >>= asSingle >>= asFunction >>= ($ [JSON v]) >>= asBool

asSingle :: Show a => [a] -> Either String a
asSingle [x] = Right x
asSingle xs = Left ("Expected one result, found " ++ show xs)

asNumber :: Value -> Either String Scientific
asNumber v = case v of
  JSON (J.Number n) -> Right n
  _ -> Left ("Expected a number, found " ++ show v)

asBool :: Value -> Either String Bool
asBool v = case v of
  JSON (J.Bool n) -> Right n
  _ -> Left ("Expected true or false, found " ++ show v)

asString :: Value -> Either String Text
asString v = case v of
  JSON (J.String n) -> Right n
  _ -> Left ("Expected a string, found " ++ show v)

numOp :: (Scientific -> Scientific) -> J.Value -> J.Value
numOp f = \case
  (J.Number n) -> (J.Number (f n))
  v -> v

stringOp :: (Text -> Text) -> J.Value -> J.Value
stringOp f = \case
  (J.String n) -> (J.String (f n))
  v -> v

numBinop :: (Scientific -> Scientific -> Scientific) -> Value -> Value -> Either String Value
numBinop f l r = JSON . J.Number <$> (f <$> asNumber l <*> asNumber r)

cmpOp :: (J.Value -> J.Value -> Bool) -> Value -> Value -> Either String Value
cmpOp f l r = JSON . J.Bool <$> (f <$> asJSON l <*> asJSON r)

boolBinop :: (Bool -> Bool -> Bool) -> Value -> Value -> Either String Value
boolBinop f l r = case (l, r) of
  (Function fl, Function fr) -> Right . Function $ \a ->
    JSON . J.Bool <$> (f <$> (asBool =<< fl a) <*> (asBool =<< fr a))
  (Function fl, JSON _) -> Right . Function $ \a ->
    JSON . J.Bool <$> (f <$> (asBool =<< fl a) <*> (asBool r))
  (JSON _, Function fr) -> Right . Function $ \a ->
    JSON . J.Bool <$> (f <$> (asBool l) <*> (asBool =<< fr a))
  (JSON _, JSON _) -> JSON . J.Bool <$> (f <$> asBool l <*> asBool r)

stringBinop :: (Text -> Text -> Text) -> Value -> Value -> Either String Value
stringBinop f l r = JSON . J.String <$> (f <$> asString l <*> asString r)

applyOp :: S.Binop -> Value -> Value -> Either String Value
applyOp op l r = case op of
  S.Plus -> numBinop (+) l r
  S.Minus -> numBinop (-) l r
  S.Times -> numBinop (*) l r
  S.Divide -> numBinop (/) l r
  S.Eq -> cmpOp (==) l r
  S.Neq -> cmpOp (/=) l r
  S.Gt -> cmpOp (>) l r
  S.Lt -> cmpOp (<) l r
  S.Gte -> cmpOp (>=) l r
  S.Lte -> cmpOp (<=) l r
  S.And -> boolBinop (&&) l r
  S.Or -> boolBinop (||) l r
  S.Concat -> stringBinop (<>) l r

applyUnop :: S.Unop -> Value -> Either String Value
applyUnop op x = case op of
  S.Not -> case x of
    JSON _ -> JSON . J.Bool . not <$> asBool x
    Function f -> Right . Function $ (\a -> JSON . J.Bool . not <$> (asBool =<< f a))
