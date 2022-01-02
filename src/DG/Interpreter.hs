{-# LANGUAGE LambdaCase #-}

module DG.Interpreter (evaluate, initialContext) where

import Control.Monad (filterM, join, (<=<))
import DG.BuiltinFunctions (isArray, isBool, isNull, isNumber, isObject, isString)
import DG.Runtime (Function, Value (..))
import qualified DG.Syntax as S
import qualified Data.Aeson as J
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.HashMap.Lazy ((!?))
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
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

lookupValue :: Context -> S.Identifier -> Either String J.Value
lookupValue ctx var = lookupCtx ctx var >>= \case JSON v -> pure v; v -> Left ("Expected JSON value for " ++ show var ++ " but found " ++ show v)

lookupFunction :: Context -> S.Identifier -> Either String Function
lookupFunction ctx var = lookupCtx ctx var >>= \case Function v -> pure v; _ -> Left (show var ++ " is not a function")

get :: Context -> S.Selector -> J.Value -> Either String [J.Value]
get ctx s v = case s of
  S.Field (S.Identifier name) -> case v of
    J.Object o -> Right (toList (o !? name))
    _ -> pure []
  S.Slice slice mFilter -> case v of
    J.Array a ->
      case slice of
        S.Index i ->
          if i >= 0 && i < V.length a
            then evaluateMaybeFilter ctx mFilter (a ! i) <&> \filterPassed -> [a ! i | filterPassed]
            else pure []
        S.Range from to _ ->
          let efrom = fromMaybe 0 from
              rlen = fromMaybe (V.length a) to - efrom
           in filterM (evaluateMaybeFilter ctx mFilter) (toList (V.slice efrom rlen a))
    _ -> pure []
  S.Compose s1 s2 -> do
    r1s <- get ctx s1 v
    concat <$> traverse (get ctx s2) r1s

delete :: Context -> S.Selector -> J.Value -> Either String J.Value
delete ctx s v = case s of
  S.Field (S.Identifier name) -> case v of
    J.Object o -> Right (J.Object (HM.delete name o))
    _ -> pure v
  S.Slice slice mFilter -> case v of
    J.Array a ->
      case slice of
        S.Index i ->
          if i >= 0 && i < V.length a
            then
              evaluateMaybeFilter ctx mFilter (a ! i) <&> \filterPassed ->
                if filterPassed
                  then J.Array (let (a1, rs) = V.splitAt i a in a1 <> V.drop 1 rs)
                  else v
            else pure v
        S.Range from to _ ->
          J.Array . V.fromList . fmap snd
            <$> filterM
              ( \(i, x) ->
                  if i >= fromMaybe 0 from && i < fromMaybe (V.length a) to
                    then not <$> evaluateMaybeFilter ctx mFilter x
                    else pure True
              )
              ([0 ..] `zip` V.toList a)
    _ -> pure v
  S.Compose s1 s2 -> tmap ctx s1 (delete ctx s2) v

tmap :: Context -> S.Selector -> (J.Value -> Either String J.Value) -> J.Value -> Either String J.Value
tmap ctx s f v = case s of
  S.Field (S.Identifier name) -> case v of
    J.Object o -> case o !? name of
      Nothing -> pure (J.Object o)
      Just vk -> f vk <&> \v' -> J.Object (HM.insert name v' o)
    _ -> pure v
  S.Slice slice mFilter -> case v of
    J.Array a ->
      case slice of
        S.Index i ->
          if i >= 0 && i < V.length a
            then
              evaluateMaybeFilter ctx mFilter (a ! i) >>= \filterPassed ->
                if filterPassed
                  then f (a ! i) <&> \vi -> J.Array (a // [(i, vi)])
                  else pure v
            else pure v
        S.Range from to _ ->
          let inRange i = maybe True (i >=) from && maybe True (i <) to
              mapElement i x =
                evaluateMaybeFilter ctx mFilter x >>= \filterPassed ->
                  if filterPassed && inRange i then f x else pure x
           in J.Array <$> V.imapM mapElement a
    _ -> pure v
  S.Compose s1 s2 -> tmap ctx s1 (tmap ctx s2 f) v

evaluateFunction :: Context -> S.Expr -> Either String Function
evaluateFunction ctx e = case e of
  S.Variable var -> lookupFunction ctx var
  S.Abstraction params expr -> Right $ \args ->
    if length args /= length params
      then Left ("Expected " ++ show (length params) ++ " arguments, found " ++ show (length args))
      else JSON <$> (asSingle =<< evaluate (M.fromList (params `zip` args) `M.union` ctx) expr)
  _ -> Left ("Not a function '" ++ show e ++ "'")

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
  S.Unop op expr -> pure <$> (evaluate ctx expr >>= asSingle >>= applyUnop op)
  S.Apply fExpr pExprs -> do
    f <- evaluateFunction ctx fExpr
    parameters <- traverse (asSingle <=< evaluate ctx) pExprs
    f (JSON <$> parameters) >>= \case
      JSON v -> Right [v]
      Function _ -> Left "Expected a value, found <function>"
  S.Abstraction params expr -> Left ("Expected a value, found 'as " ++ intercalate ", " (show <$> params) ++ " in " ++ show expr ++ "'")
  S.Selection expr selector operation -> do
    v <- evaluate ctx expr
    case operation of
      S.Get -> concat <$> traverse (get ctx selector) v
      S.Delete -> traverse (delete ctx selector) v
      S.Set ex ->
        evaluate ctx ex >>= asSingle >>= \x -> traverse (tmap ctx selector (Right . const x)) v
      S.SetAs var ex ->
        traverse (tmap ctx selector (\x -> evaluate (M.insert var (JSON x) ctx) ex >>= asSingle)) v
      S.PlusEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> traverse (tmap ctx selector (Right . numOp (+ x))) v
      S.MinusEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> traverse (tmap ctx selector (Right . numOp (subtract x))) v
      S.TimesEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> traverse (tmap ctx selector (Right . numOp (* x))) v
      S.DivEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> traverse (tmap ctx selector (Right . numOp (/ x))) v
      S.ConcatEq ex ->
        evaluate ctx ex >>= asSingle >>= asString >>= \x -> traverse (tmap ctx selector (Right . stringOp (<> x))) v

evaluateFilter :: Context -> S.Filter -> J.Value -> Either String Bool
evaluateFilter ctx (S.Filter var expr) v =
  asBool =<< asSingle =<< evaluate (M.insert var (JSON v) ctx) expr

evaluateMaybeFilter :: Context -> Maybe S.Filter -> J.Value -> Either String Bool
evaluateMaybeFilter ctx mf v = case mf of
  Nothing -> Right True
  Just f -> evaluateFilter ctx f v

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

applyUnop :: S.Unop -> J.Value -> Either String J.Value
applyUnop op x = case op of
  S.Not -> J.Bool . not <$> asBool x
