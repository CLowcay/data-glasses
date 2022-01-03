{-# LANGUAGE LambdaCase #-}

module DG.Interpreter (evaluate, initialContext) where

import Control.Monad (join, (<=<))
import DG.BuiltinFunctions (isArray, isBool, isNull, isNumber, isObject, isString)
import DG.Runtime (Value (..), asFunction, asJSON)
import qualified DG.Syntax as S
import qualified Data.Aeson as J
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.HashMap.Lazy ((!?))
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
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

delete :: Context -> S.Selector -> J.Value -> Either String J.Value
delete ctx s v = case s of
  S.Field (S.Identifier name) -> case v of
    J.Object o -> Right (J.Object (HM.delete name o))
    _ -> pure v
  S.Slice slice -> case v of
    J.Array a ->
      case slice of
        S.Index i ->
          if i >= 0 && i < V.length a
            then pure (J.Array (let (a1, rs) = V.splitAt i a in a1 <> V.drop 1 rs))
            else pure v
        S.Range from to _ ->
          Right (J.Array (V.ifilter (\i _ -> i >= fromMaybe 0 from && i < fromMaybe (V.length a) to) a))
    _ -> pure v
  S.Where fexpr -> undefined
  S.Compose s1 s2 -> tmap ctx s1 (delete ctx s2) v

tmap :: Context -> S.Selector -> (J.Value -> Either String J.Value) -> J.Value -> Either String J.Value
tmap ctx s f v = case s of
  S.Field (S.Identifier name) -> case v of
    J.Object o -> case o !? name of
      Nothing -> pure (J.Object o)
      Just vk -> f vk <&> \v' -> J.Object (HM.insert name v' o)
    _ -> pure v
  S.Slice slice -> case v of
    J.Array a ->
      case slice of
        S.Index i ->
          if i >= 0 && i < V.length a
            then f (a ! i) <&> \vi -> J.Array (a // [(i, vi)])
            else pure v
        S.Range from to _ ->
          let inRange i = maybe True (i >=) from && maybe True (i <) to
              mapElement i x = if inRange i then f x else pure x
           in J.Array <$> V.imapM mapElement a
    _ -> pure v
  S.Where fexpr -> evaluateFilter ctx fexpr v >>= \passed -> if passed then f v else pure v
  S.Compose s1 s2 -> tmap ctx s1 (tmap ctx s2 f) v

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
      S.Delete -> traverse (fmap JSON . delete ctx selector) v
      S.Set ex ->
        evaluate ctx ex >>= asSingle >>= asJSON >>= \x -> traverse (fmap JSON . tmap ctx selector (Right . const x)) v
      S.SetAs var ex ->
        traverse (fmap JSON . tmap ctx selector (\x -> evaluate (M.insert var (JSON x) ctx) ex >>= asSingle >>= asJSON)) v
      S.PlusEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> traverse (fmap JSON . tmap ctx selector (Right . numOp (+ x))) v
      S.MinusEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> traverse (fmap JSON . tmap ctx selector (Right . numOp (subtract x))) v
      S.TimesEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> traverse (fmap JSON . tmap ctx selector (Right . numOp (* x))) v
      S.DivEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> traverse (fmap JSON . tmap ctx selector (Right . numOp (/ x))) v
      S.ConcatEq ex ->
        evaluate ctx ex >>= asSingle >>= asString >>= \x -> traverse (fmap JSON . tmap ctx selector (Right . stringOp (<> x))) v

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
boolBinop f l r = JSON . J.Bool <$> (f <$> asBool l <*> asBool r)

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
  S.Not -> JSON . J.Bool . not <$> asBool x
