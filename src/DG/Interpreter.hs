{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module DG.Interpreter (evaluate, initialContext) where

import Control.Exception (SomeException)
import Control.Monad (foldM, (<=<), (>=>))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans (lift)
import DG.BuiltinFunctions (asArrayCollector, concatCollector, countCollector, intersectionCollector, isArray, isBool, isNull, isNumber, isObject, isString, joinCollector, meanCollector, productCollector, stringify, sumCollector, unionCollector)
import DG.Json (JsonE (..), Object)
import DG.Runtime (Function (F), JsonMeta (..), JsonValue, Value (..), asFunction, asJSON, runtimeError, withCollector)
import qualified DG.Syntax as DG
import Data.Bifunctor (first, second)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Text (Text)
import Data.Vector ((!), (//))
import qualified Data.Vector as V
import Streamly.Prelude (SerialT)
import qualified Streamly.Prelude as S

type Context = Map DG.Identifier Value

initialContext :: Context
initialContext =
  M.fromList
    ( ( second Function
          <$> [isNull, isBool, isString, isNumber, isArray, isObject, stringify, joinCollector]
      )
        ++ (second Collector <$> [sumCollector, productCollector, countCollector])
        ++ (second Collector <$> [asArrayCollector, concatCollector])
        ++ [second Collector meanCollector]
        ++ (second Collector <$> [unionCollector, intersectionCollector])
    )

lookupCtx :: Context -> DG.Identifier -> Either String Value
lookupCtx ctx var = maybe (Left ("Undefined variable " ++ show var)) Right (M.lookup var ctx)

data Key = StringKey Text | IndexKey Int deriving (Eq, Ord, Show)

type Element = (Maybe Key, JsonValue)

type Stream a = SerialT (Either SomeException) a

mapStream :: (a -> Either SomeException b) -> Stream a -> Stream b
mapStream f s = s >>= lift . f

traverseBase :: Stream a -> (a -> Either SomeException b) -> Stream b
traverseBase s f = s >>= lift . f

get :: Context -> NonEmpty DG.Selector -> Stream Element -> Stream Element
get ctx (s1 :| ss) vs = foldl (flip (get1 ctx)) (get1 ctx s1 vs) ss

get1 :: Context -> DG.Selector -> Stream Element -> Stream Element
get1 ctx s vs = case s of
  DG.Field (DG.Identifier name) -> S.mapMaybe (\case Object o -> nameWithKey name o; _ -> Nothing) (snd <$> vs)
  DG.Slice slices -> mconcat (getSlice <$> slices)
  DG.Where fexpr -> do
    f <- lift (evaluateFilterFunction ctx fexpr)
    S.filterM f vs
  DG.Map mexpr -> do
    F arity f <- lift (asFunction =<< asSingle (evaluate ctx mexpr))
    case arity of
      1 -> traverseBase vs (traverse (asJSON <=< f . pure . JSON))
      2 ->
        traverseBase vs $ \(key, value) -> do
          k <- maybe (runtimeError "No key") pure key
          (key,) <$> (asJSON =<< f [keyToValue k, JSON value])
      _ -> runtimeError ("Invalid filter function.  Expected function with 1 or 2 parameters, found " ++ show arity)
  DG.Collect cexpr -> do
    collector <- lift (asSingle (evaluate ctx cexpr))
    withCollector collector $ \(unit, inj, proj, op) ->
      lift ((Nothing,) . proj <$> S.foldlM' op (pure unit) (mapStream inj (snd <$> vs)))
  where
    nameWithKey name o = (Just (StringKey name),) <$> (o !? name)
    getSlice = \case
      DG.Index expr ->
        let ixs = evaluate ctx expr
         in vs >>= \(_, value) -> case value of
              Array a -> S.mapMaybe (\i -> (Just (IndexKey i),) <$> (a V.!? i)) (asInt =<< ixs)
              Object o -> S.mapMaybe (\k -> (Just (StringKey k),) <$> (o !? k)) (asString =<< ixs)
              _ -> S.nil
      DG.Range from to _ ->
        vs >>= \(_, value) -> case value of
          Array a ->
            let efrom = fromMaybe 0 from
                rlen = fromMaybe (V.length a) to - efrom
             in S.fromList ((Just . IndexKey <$> [efrom ..]) `zip` V.toList (V.slice efrom rlen a))
          Object o | isNothing from && isNothing to -> S.fromList (first (Just . StringKey) <$> M.toList o)
          _ -> S.nil

tmap ::
  Context ->
  NonEmpty DG.Selector ->
  (Element -> Either SomeException JsonValue) ->
  Stream Element ->
  Stream Value
tmap ctx s f = mapStream (fmap JSON . tmapM ctx s f)

tmapM :: Context -> NonEmpty DG.Selector -> (Element -> Either SomeException JsonValue) -> Element -> Either SomeException JsonValue
tmapM ctx s0 f = go s0
  where
    go (s :| []) = tmapM1 ctx s f
    go (s :| (s1 : ss)) = tmapM1 ctx s (go (s1 :| ss))

tmapM1 ::
  Context ->
  DG.Selector ->
  (Element -> Either SomeException JsonValue) ->
  Element ->
  Either SomeException JsonValue
tmapM1 ctx s f element@(_, value) = case s of
  DG.Field (DG.Identifier name) ->
    case value of Object o -> Object <$> updateObject o name; _ -> pure value
  DG.Slice slices -> foldM updateSlice value slices
  DG.Where fexpr ->
    evaluateFilter ctx fexpr element
      >>= \passed -> if passed then f element else pure value
  DG.Map _ -> runtimeError "Cannot modify mapped value"
  DG.Collect _ -> runtimeError "Cannot modify collected value"
  where
    updateObject obj name = case obj !? name of
      (Just vOld) -> f (Just (StringKey name), vOld) <&> \v' -> M.insert name v' obj
      _ -> pure obj
    updateSlice v slice = case slice of
      DG.Index expr ->
        let ixs = evaluate ctx expr
         in case v of
              Array a -> do
                is <- S.toList (S.filter (\i -> i >= 0 && i < V.length a) (asInt =<< ixs))
                Array . (a //) <$> traverse (\i -> (i,) <$> f (Just (IndexKey i), a ! i)) is
              Object o -> Object <$> S.foldlM' updateObject (pure o) (asString =<< ixs)
              _ -> pure v
      DG.Range from to _ ->
        case v of
          Array a ->
            let inRange i = maybe True (i >=) from && maybe True (i <) to
                mapElement i x = if inRange i then f (Just (IndexKey i), x) else pure x
             in Array <$> V.imapM mapElement a
          Object o | isNothing from && isNothing to -> Object <$> foldM updateObject o (M.keys o)
          _ -> pure v

streamToMap :: (Monad m, Ord k) => SerialT m (k, v) -> m (Map k v)
streamToMap = S.foldl' (\m (k, v) -> M.insert k v m) M.empty

evaluate :: Context -> DG.Expr -> Stream Value
evaluate ctx e = case e of
  DG.Variable var -> S.fromFoldable (lookupCtx ctx var)
  DG.StringLit text -> pure (JSON (String text))
  DG.NumLit n -> pure (JSON (Number (fromIntegral n)))
  DG.NullLit -> pure (JSON Null)
  DG.BoolLit b -> pure (JSON (Bool b))
  DG.Array as -> lift $ do
    JSON . Array . V.fromList <$> S.toList (asJSON =<< mconcat (evaluate ctx <$> as))
  DG.Object fields ->
    lift (JSON . Object <$> streamToMap (mconcat (evaluateField ctx <$> fields)))
  DG.Binop op lExpr rExpr -> lift $ do
    l <- asSingle (evaluate ctx lExpr)
    r <- asSingle (evaluate ctx rExpr)
    applyOp op l r
  DG.Unop op expr -> lift (applyUnop op =<< asSingle (evaluate ctx expr))
  DG.Apply fExpr pExprs -> lift $ do
    F _ f <- asFunction =<< asSingle (evaluate ctx fExpr)
    parameters <- S.toList (mconcat (evaluate ctx <$> pExprs))
    f parameters
  DG.Abstraction params expr ->
    let nParams = length params
     in pure . Function . F nParams $ \args ->
          if length args /= nParams
            then runtimeError ("Expected " ++ show nParams ++ " arguments, found " ++ show (length args))
            else asSingle (evaluate (M.fromList (params `zip` args) `M.union` ctx) expr)
  DG.If cond eThen eElse -> do
    d <- lift (asBool =<< asSingle (evaluate ctx cond))
    if d then evaluate ctx eThen else evaluate ctx eElse
  DG.Let definitions expr -> do
    let evaluateAndExtend ctx' (var, definition) =
          asSingle (evaluate ctx' definition) <&> \v -> M.insert var v ctx'
    extendedCtx <- lift (foldM evaluateAndExtend ctx definitions)
    evaluate extendedCtx expr
  DG.Selection expr selector operation ->
    let vs = (fmap (Nothing,) . asJSON) =<< evaluate ctx expr
     in case operation of
          DG.Get -> JSON . snd <$> get ctx selector vs
          DG.Delete -> tmap ctx selector (pure . const (Extention Tombstone)) vs
          DG.Set ex -> do
            v' <- lift (asJSON =<< asSingle (evaluate ctx ex))
            tmap ctx selector (pure . const v') vs
          DG.SetAs var ex ->
            let f (_, x) = asJSON =<< asSingle (evaluate (M.insert var (JSON x) ctx) ex)
             in tmap ctx selector f vs
          DG.SetAsI keyVar var ex ->
            let f (k, x) = do
                  ctx' <- case keyToValue <$> k of
                    Nothing -> runtimeError "No key"
                    Just keyValue -> Right (M.insert keyVar keyValue (M.insert var (JSON x) ctx))
                  asJSON =<< asSingle (evaluate ctx' ex)
             in tmap ctx selector f vs
          DG.PlusEq ex -> do
            v' <- lift (asNumber =<< asSingle (evaluate ctx ex))
            tmap ctx selector (pure . numOp (+ v') . snd) vs
          DG.MinusEq ex -> do
            v' <- lift (asNumber =<< asSingle (evaluate ctx ex))
            tmap ctx selector (pure . numOp (subtract v') . snd) vs
          DG.TimesEq ex -> do
            v' <- lift (asNumber =<< asSingle (evaluate ctx ex))
            tmap ctx selector (pure . numOp (* v') . snd) vs
          DG.DivEq ex -> do
            v' <- lift (asNumber =<< asSingle (evaluate ctx ex))
            tmap ctx selector (pure . numOp (/ v') . snd) vs
          DG.ConcatEq ex -> do
            v' <- lift (asString =<< asSingle (evaluate ctx ex))
            tmap ctx selector (pure . stringOp (<> v') . snd) vs

evaluateField :: Context -> DG.ObjectElement -> Stream (Text, JsonValue)
evaluateField ctx = \case
  DG.SimpleElement key e -> lift ((key,) <$> (asJSON =<< asSingle (evaluate ctx e)))
  DG.ExprElement keyExpr e -> do
    value <- lift (asJSON =<< asSingle (evaluate ctx e))
    (,value) <$> mapStream asString (evaluate ctx keyExpr)
  DG.ExprAsElement keyExpr key e ->
    evaluate ctx keyExpr >>= asString >>= \k ->
      (k,) <$> lift (asJSON =<< asSingle (evaluate (M.insert key (JSON (String k)) ctx) e))

evaluateFilter :: Context -> DG.Expr -> (Maybe Key, JsonValue) -> Either SomeException Bool
evaluateFilter ctx expr v = evaluateFilterFunction ctx expr >>= ($ v)

keyToValue :: Key -> Value
keyToValue = \case StringKey k -> JSON (String k); IndexKey i -> JSON (Number (fromIntegral i))

evaluateFilterFunction :: Context -> DG.Expr -> Either SomeException ((Maybe Key, JsonValue) -> Either SomeException Bool)
evaluateFilterFunction ctx expr =
  asSingle (evaluate ctx expr) >>= asFunction
    <&> ( \(F arity f) (key, v) ->
            case arity of
              1 -> f [JSON v] >>= asBool
              2 -> do
                k <- maybe (runtimeError "No key") pure key
                f [keyToValue k, JSON v] >>= asBool
              _ -> runtimeError ("Filter function must have one or two parameters, found " ++ show arity)
        )

asSingle :: Show a => Stream a -> Either SomeException a
asSingle =
  S.uncons >=> \case
    Nothing -> runtimeError "No results"
    Just (v, s') -> do
      isEmpty <- S.null s'
      if isEmpty then pure v else runtimeError "Multiple results not supported here"

asNumber :: MonadThrow m => Value -> m Scientific
asNumber v = case v of
  JSON (Number n) -> pure n
  _ -> runtimeError ("Expected a number, found " ++ show v)

asInt :: MonadThrow m => Value -> m Int
asInt = asNumber >=> maybe (runtimeError "Index out of range") pure . toBoundedInteger

asObject :: MonadThrow m => Value -> m (Object JsonMeta)
asObject v = case v of
  JSON (Object o) -> pure o
  _ -> runtimeError ("Expected an object, found " ++ show v)

asBool :: MonadThrow m => Value -> m Bool
asBool v = case v of
  JSON (Bool n) -> pure n
  _ -> runtimeError ("Expected true or false, found " ++ show v)

asString :: MonadThrow m => Value -> m Text
asString v = case v of
  JSON (String n) -> pure n
  _ -> runtimeError ("Expected a string, found " ++ show v)

numOp :: (Scientific -> Scientific) -> JsonValue -> JsonValue
numOp f = \case
  (Number n) -> (Number (f n))
  v -> v

stringOp :: (Text -> Text) -> JsonValue -> JsonValue
stringOp f = \case
  (String n) -> (String (f n))
  v -> v

numBinop :: (Scientific -> Scientific -> Scientific) -> Value -> Value -> Either SomeException Value
numBinop f l r = JSON . Number <$> (f <$> asNumber l <*> asNumber r)

intBinop :: (Int -> Int -> Int) -> Value -> Value -> Either SomeException Value
intBinop f l r = JSON . Number . fromIntegral <$> (f <$> asInt l <*> asInt r)

objectBinop :: (Object JsonMeta -> Object JsonMeta -> Object JsonMeta) -> Value -> Value -> Either SomeException Value
objectBinop f l r = JSON . Object <$> (f <$> asObject l <*> asObject r)

cmpOp :: (JsonValue -> JsonValue -> Bool) -> Value -> Value -> Either SomeException Value
cmpOp f l r = JSON . Bool <$> (f <$> asJSON l <*> asJSON r)

boolBinop :: (Bool -> Bool -> Bool) -> Value -> Value -> Either SomeException Value
boolBinop f l r = case (l, r) of
  (Function (F 1 fl), Function (F 1 fr)) -> Right . Function . F 1 $ \a ->
    JSON . Bool <$> (f <$> (asBool =<< fl a) <*> (asBool =<< fr a))
  (Function (F 1 fl), JSON _) -> Right . Function . F 1 $ \a ->
    JSON . Bool <$> (f <$> (asBool =<< fl a) <*> asBool r)
  (JSON _, Function (F 1 fr)) -> Right . Function . F 1 $ \a ->
    JSON . Bool <$> (f <$> asBool l <*> (asBool =<< fr a))
  (JSON _, JSON _) -> JSON . Bool <$> (f <$> asBool l <*> asBool r)
  _ -> runtimeError ("Expected JSON or function of one argument, found " ++ show l ++ " and " ++ show r)

applyOp :: DG.Binop -> Value -> Value -> Either SomeException Value
applyOp op l r = case op of
  DG.Plus -> numBinop (+) l r
  DG.Minus -> numBinop (-) l r
  DG.Times -> numBinop (*) l r
  DG.Divide -> numBinop (/) l r
  DG.Modulo -> intBinop mod l r
  DG.Eq -> cmpOp (==) l r
  DG.Neq -> cmpOp (/=) l r
  DG.Gt -> cmpOp (>) l r
  DG.Lt -> cmpOp (<) l r
  DG.Gte -> cmpOp (>=) l r
  DG.Lte -> cmpOp (<=) l r
  DG.And -> boolBinop (&&) l r
  DG.Or -> boolBinop (||) l r
  DG.Concat -> case (l, r) of
    (JSON (String sl), JSON (String sr)) -> pure (JSON (String (sl <> sr)))
    (JSON (Array al), JSON (Array ar)) -> pure (JSON (Array (al <> ar)))
    _ -> runtimeError ("Expected strings or arrays for ++, found " ++ show l ++ " ++ " ++ show r)
  DG.Union -> objectBinop M.union l r
  DG.Intersection -> objectBinop M.intersection l r
  DG.Difference -> objectBinop M.difference l r

applyUnop :: DG.Unop -> Value -> Either SomeException Value
applyUnop op x = case op of
  DG.Not -> case x of
    JSON _ -> JSON . Bool . not <$> asBool x
    Function (F 1 f) -> Right . Function . F 1 $ (\a -> JSON . Bool . not <$> (asBool =<< f a))
    _ -> runtimeError ("Expected JSON or function of one argument, found " ++ show x)
