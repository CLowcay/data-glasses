{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module DG.Interpreter (evaluate, initialContext) where

import Control.Monad (filterM, foldM, join, (<=<), (>=>))
import DG.BuiltinFunctions (asArrayCollector, concatCollector, countCollector, intersectionCollector, isArray, isBool, isNull, isNumber, isObject, isString, joinCollector, meanCollector, productCollector, stringify, sumCollector, unionCollector)
import DG.Runtime (Function (F), JSONF (..), Value (..), asFunction, asJSON, fromJSONM, toJSONM, withCollector)
import qualified DG.Syntax as S
import qualified Data.Aeson as J
import Data.Bifunctor (first, second)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.HashMap.Lazy ((!?))
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Vector ((!), (//))
import qualified Data.Vector as V

type Context = Map S.Identifier Value

initialContext :: Context
initialContext =
  M.fromList
    ( ( second Function
          <$> [isNull, isBool, isString, isNumber, isObject, isArray, stringify, joinCollector]
      )
        ++ (second Collector <$> [sumCollector, productCollector, countCollector])
        ++ (second Collector <$> [asArrayCollector, concatCollector])
        ++ [second Collector meanCollector]
        ++ (second Collector <$> [unionCollector, intersectionCollector])
    )

lookupCtx :: Context -> S.Identifier -> Either String Value
lookupCtx ctx var = maybe (Left ("Undefined variable " ++ show var)) Right (M.lookup var ctx)

get :: Context -> S.Selector -> [(Maybe Key, J.Value)] -> Either String [(Maybe Key, J.Value)]
get ctx s vs = case s of
  S.Field (S.Identifier name) ->
    fmap concat . for (snd <$> vs) $ \case J.Object o -> Right (toList ((Just (StringKey name),) <$> o !? name)); _ -> pure []
  S.Slice slices -> concat <$> traverse getSlice slices
  S.Where fexpr -> do
    f <- evaluateFilterFunction ctx fexpr
    filterM f vs
  S.Map mexpr -> do
    F arity f <- asFunction =<< asSingle =<< evaluate ctx mexpr
    case arity of
      1 -> mapM (traverse (asJSON <=< f . pure . JSON)) vs
      2 ->
        mapM
          ( \(key, vk) -> do
              k <- maybe (Left "No key") pure key
              (key,) <$> (asJSON =<< f [keyToValue k, JSON vk])
          )
          vs
      _ -> Left ("Invalid filter function.  Expected function with 1 or 2 parameters, found " ++ show arity)
  S.Collect cexpr -> do
    collector <- asSingle =<< evaluate ctx cexpr
    withCollector collector $ \(unit, inj, proj, op) ->
      pure . (Nothing,) . proj <$> (foldM op unit =<< traverse inj (snd <$> vs))
  S.Compose s1 s2 -> get ctx s2 =<< get ctx s1 vs
  where
    getSlice = \case
      S.Index expr -> do
        ixs <- evaluate ctx expr
        fmap concat . for (snd <$> vs) $ \case
          J.Array a -> mapMaybe (\i -> (Just (IndexKey i),) <$> a V.!? i) <$> traverse asInt ixs
          J.Object o -> mapMaybe (\k -> (Just (StringKey k),) <$> o !? k) <$> traverse asString ixs
          _ -> pure []
      S.Range from to _ ->
        fmap concat . for (snd <$> vs) $ \case
          J.Array a ->
            let efrom = fromMaybe 0 from
                rlen = fromMaybe (V.length a) to - efrom
             in Right ((Just . IndexKey <$> [efrom ..]) `zip` V.toList (V.slice efrom rlen a))
          J.Object o | isNothing from && isNothing to -> Right (first (Just . StringKey) <$> HM.toList o)
          _ -> pure []

tmap :: Context -> S.Selector -> ((Maybe Key, J.Value) -> Either String J.Value) -> [(Maybe Key, J.Value)] -> Either String [Value]
tmap ctx s f v = mapMaybe (fmap (JSON . fromJSONM)) <$> traverse (tmapM ctx s (\(k, vk) -> Just . toJSONM <$> f (k, fromJSONM vk))) (second toJSONM <$> v)

tmapM ::
  Context ->
  S.Selector ->
  ((Maybe Key, JSONF Maybe) -> Either String (Maybe (JSONF Maybe))) ->
  (Maybe Key, JSONF Maybe) ->
  Either String (Maybe (JSONF Maybe))
tmapM ctx s f (key, v) = case s of
  S.Field (S.Identifier name) ->
    Just <$> case v of
      Object o -> Object <$> updateObject o name
      _ -> pure v
  S.Slice slices -> foldM updateSlice (Just v) slices
  S.Where fexpr ->
    evaluateFilter ctx fexpr (key, fromJSONM v)
      >>= \passed -> if passed then f (key, v) else pure (Just v)
  S.Map _ -> Left "Cannot modify mapped value"
  S.Collect _ -> Left "Cannot modify collected value"
  S.Compose s1 s2 -> tmapM ctx s1 (tmapM ctx s2 f) (key, v)
  where
    updateObject obj name = case obj !? name of
      Just (Just vOld) -> f (Just (StringKey name), vOld) <&> \v' -> HM.insert name v' obj
      _ -> pure obj
    updateSlice mv slice =
      for mv $ \v -> case slice of
        S.Index expr -> do
          ixs <- evaluate ctx expr
          case v of
            Array a -> do
              is <- filter (\i -> i >= 0 && i < V.length a) <$> traverse asInt ixs
              Array . (a //) <$> traverse (\i -> (i,) <$> maybe (pure Nothing) f ((Just (IndexKey i),) <$> a ! i)) is
            Object o -> Object <$> (foldM updateObject o =<< traverse asString ixs)
            _ -> pure v
        S.Range from to _ ->
          case v of
            Array a ->
              let inRange i = maybe True (i >=) from && maybe True (i <) to
                  mapElement i = \case Just x -> if inRange i then f (Just (IndexKey i), x) else pure (Just x); Nothing -> pure Nothing
               in Array <$> V.imapM mapElement a
            Object o | isNothing from && isNothing to -> Object <$> foldM updateObject o (HM.keys o)
            _ -> pure v

evaluate :: Context -> S.Expr -> Either String [Value]
evaluate ctx e = case e of
  S.Variable var -> pure <$> lookupCtx ctx var
  S.StringLit text -> Right [JSON (J.String text)]
  S.NumLit n -> Right [JSON (J.Number (fromIntegral n))]
  S.NullLit -> Right [JSON J.Null]
  S.BoolLit b -> Right [JSON (J.Bool b)]
  S.Array as -> pure . JSON . J.Array . V.fromList . join <$> traverse (traverse asJSON <=< evaluate ctx) as
  S.Object fields -> do
    evaluatedFields <- traverse (evaluateField ctx) fields
    pure [JSON (J.Object (HM.fromList (concat evaluatedFields)))]
  S.Binop op lExpr rExpr -> do
    l <- asSingle =<< evaluate ctx lExpr
    r <- asSingle =<< evaluate ctx rExpr
    pure <$> applyOp op l r
  S.Unop op expr -> pure <$> (evaluate ctx expr >>= asSingle >>= applyUnop op)
  S.Apply fExpr pExprs -> do
    F _ f <- asFunction =<< asSingle =<< evaluate ctx fExpr
    parameters <- concat <$> traverse (evaluate ctx) pExprs
    pure <$> f parameters
  S.Abstraction params expr ->
    let nParams = length params
     in Right . pure . Function . F nParams $ \args ->
          if length args /= nParams
            then Left ("Expected " ++ show nParams ++ " arguments, found " ++ show (length args))
            else asSingle =<< evaluate (M.fromList (params `zip` args) `M.union` ctx) expr
  S.If cond eThen eElse -> do
    d <- evaluate ctx cond >>= asSingle >>= asBool
    if d then evaluate ctx eThen else evaluate ctx eElse
  S.Let definitions expr -> do
    let evaluateAndExtend ctx' (var, definition) =
          evaluate ctx' definition >>= asSingle <&> \v -> M.insert var v ctx'
    extendedCtx <- foldM evaluateAndExtend ctx definitions
    evaluate extendedCtx expr
  S.Selection expr selector operation -> do
    v <- traverse (fmap (Nothing,) . asJSON) =<< evaluate ctx expr
    case operation of
      S.Get -> fmap (JSON . snd) <$> get ctx selector v
      S.Delete ->
        mapMaybe (fmap (JSON . fromJSONM)) <$> traverse (tmapM ctx selector (const (Right Nothing))) (second toJSONM <$> v)
      S.Set ex ->
        evaluate ctx ex >>= asSingle >>= asJSON >>= \x -> tmap ctx selector (Right . const x) v
      S.SetAs var ex ->
        tmap ctx selector (\(_, x) -> evaluate (M.insert var (JSON x) ctx) ex >>= asSingle >>= asJSON) v
      S.SetAsI keyVar var ex ->
        tmap
          ctx
          selector
          ( \(k, x) -> do
              ctx' <- case keyToValue <$> k of
                Nothing -> Left "No key"
                Just keyValue -> Right (M.insert keyVar keyValue (M.insert var (JSON x) ctx))
              evaluate ctx' ex >>= asSingle >>= asJSON
          )
          v
      S.PlusEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> tmap ctx selector (Right . numOp (+ x) . snd) v
      S.MinusEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> tmap ctx selector (Right . numOp (subtract x) . snd) v
      S.TimesEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> tmap ctx selector (Right . numOp (* x) . snd) v
      S.DivEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber >>= \x -> tmap ctx selector (Right . numOp (/ x) . snd) v
      S.ConcatEq ex ->
        evaluate ctx ex >>= asSingle >>= asString >>= \x -> tmap ctx selector (Right . stringOp (<> x) . snd) v

evaluateField :: Context -> S.ObjectElement -> Either String [(Text, J.Value)]
evaluateField ctx = \case
  S.SimpleElement key e -> pure . (key,) <$> (evaluate ctx e >>= asSingle >>= asJSON)
  S.ExprElement keyExpr e -> do
    value <- asJSON =<< asSingle =<< evaluate ctx e
    traverse (fmap (,value) . asString) =<< evaluate ctx keyExpr
  S.ExprAsElement keyExpr key e ->
    evaluate ctx keyExpr
      >>= traverse
        (asString >=> (\k -> (k,) <$> (evaluate (M.insert key (JSON (J.String k)) ctx) e >>= asSingle >>= asJSON)))

evaluateFilter :: Context -> S.Expr -> (Maybe Key, J.Value) -> Either String Bool
evaluateFilter ctx expr v = evaluateFilterFunction ctx expr >>= ($v)

data Key = StringKey Text | IndexKey Int deriving (Eq, Ord, Show)

keyToValue :: Key -> Value
keyToValue = \case StringKey k -> JSON (J.String k); IndexKey i -> JSON (J.Number (fromIntegral i))

evaluateFilterFunction :: Context -> S.Expr -> Either String ((Maybe Key, J.Value) -> Either String Bool)
evaluateFilterFunction ctx expr =
  evaluate ctx expr >>= asSingle >>= asFunction
    <&> ( \(F arity f) (key, v) ->
            case arity of
              1 -> f [JSON v] >>= asBool
              2 -> do
                k <- maybe (Left "No key") pure key
                f [keyToValue k, JSON v] >>= asBool
              _ -> Left ("Filter function must have one or two parameters, found " ++ show arity)
        )

asSingle :: Show a => [a] -> Either String a
asSingle [x] = Right x
asSingle xs = Left ("Expected one result, found " ++ show xs)

asNumber :: Value -> Either String Scientific
asNumber v = case v of
  JSON (J.Number n) -> Right n
  _ -> Left ("Expected a number, found " ++ show v)

asInt :: Value -> Either String Int
asInt = asNumber >=> maybe (Left "Index out of range") pure . toBoundedInteger

asObject :: Value -> Either String J.Object
asObject v = case v of
  JSON (J.Object o) -> Right o
  _ -> Left ("Expected an object, found " ++ show v)

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

intBinop :: (Int -> Int -> Int) -> Value -> Value -> Either String Value
intBinop f l r = JSON . J.Number . fromIntegral <$> (f <$> asInt l <*> asInt r)

objectBinop :: (J.Object -> J.Object -> J.Object) -> Value -> Value -> Either String Value
objectBinop f l r = JSON . J.Object <$> (f <$> asObject l <*> asObject r)

cmpOp :: (J.Value -> J.Value -> Bool) -> Value -> Value -> Either String Value
cmpOp f l r = JSON . J.Bool <$> (f <$> asJSON l <*> asJSON r)

boolBinop :: (Bool -> Bool -> Bool) -> Value -> Value -> Either String Value
boolBinop f l r = case (l, r) of
  (Function (F 1 fl), Function (F 1 fr)) -> Right . Function . F 1 $ \a ->
    JSON . J.Bool <$> (f <$> (asBool =<< fl a) <*> (asBool =<< fr a))
  (Function (F 1 fl), JSON _) -> Right . Function . F 1 $ \a ->
    JSON . J.Bool <$> (f <$> (asBool =<< fl a) <*> asBool r)
  (JSON _, Function (F 1 fr)) -> Right . Function . F 1 $ \a ->
    JSON . J.Bool <$> (f <$> asBool l <*> (asBool =<< fr a))
  (JSON _, JSON _) -> JSON . J.Bool <$> (f <$> asBool l <*> asBool r)
  _ -> Left ("Expected JSON or function of one argument, found " ++ show l ++ " and " ++ show r)

applyOp :: S.Binop -> Value -> Value -> Either String Value
applyOp op l r = case op of
  S.Plus -> numBinop (+) l r
  S.Minus -> numBinop (-) l r
  S.Times -> numBinop (*) l r
  S.Divide -> numBinop (/) l r
  S.Modulo -> intBinop mod l r
  S.Eq -> cmpOp (==) l r
  S.Neq -> cmpOp (/=) l r
  S.Gt -> cmpOp (>) l r
  S.Lt -> cmpOp (<) l r
  S.Gte -> cmpOp (>=) l r
  S.Lte -> cmpOp (<=) l r
  S.And -> boolBinop (&&) l r
  S.Or -> boolBinop (||) l r
  S.Concat -> case (l, r) of
    (JSON (J.String sl), JSON (J.String sr)) -> pure (JSON (J.String (sl <> sr)))
    (JSON (J.Array al), JSON (J.Array ar)) -> pure (JSON (J.Array (al <> ar)))
    _ -> Left ("Expected strings or arrays for ++, found " ++ show l ++ " ++ " ++ show r)
  S.Union -> objectBinop HM.union l r
  S.Intersection -> objectBinop HM.intersection l r
  S.Difference -> objectBinop HM.difference l r

applyUnop :: S.Unop -> Value -> Either String Value
applyUnop op x = case op of
  S.Not -> case x of
    JSON _ -> JSON . J.Bool . not <$> asBool x
    Function (F 1 f) -> Right . Function . F 1 $ (\a -> JSON . J.Bool . not <$> (asBool =<< f a))
    _ -> Left ("Expected JSON or function of one argument, found " ++ show x)
