{-# LANGUAGE LambdaCase #-}

module DG.Interpreter where

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

get :: S.Selector -> J.Value -> [J.Value]
get s v = case s of
  S.Field (S.Identifier name) -> case v of
    J.Object o -> toList (o !? name)
    _ -> []
  S.Index i -> case v of
    J.Array a -> [a ! i | i >= 0 && i < V.length a]
    _ -> []
  S.Slice from to -> case v of
    J.Array a ->
      let efrom = fromMaybe 0 from
          rlen = fromMaybe (V.length a) to - efrom
       in toList (V.slice efrom rlen a)
    _ -> []
  S.Compose s1 s2 -> get s1 v >>= get s2

tmap :: S.Selector -> (J.Value -> J.Value) -> J.Value -> J.Value
tmap s f v = case s of
  S.Field (S.Identifier name) -> case v of
    J.Object o -> J.Object (HM.update (pure . f) name o)
    _ -> v
  S.Index i -> case v of
    J.Array a ->
      if i >= 0 && i < V.length a
        then J.Array (a // [(i, f (a ! i))])
        else v
    _ -> v
  S.Slice from to -> case v of
    J.Array a ->
      let inRange i = maybe True (i >=) from && maybe True (i <) to
       in J.Array (V.imap (\i x -> if inRange i then f x else x) a)
    _ -> v
  S.Compose s1 s2 -> tmap s1 (tmap s2 f) v

type Context = HashMap S.Identifier J.Value

evaluate :: Context -> S.Expr -> Either String [J.Value]
evaluate ctx e = case e of
  S.Variable var ->
    maybe (Left ("Undefined variable " ++ show var)) (Right . pure) (ctx !? var)
  S.StringLit text -> Right [J.String text]
  S.NumLit n -> Right [J.Number (fromIntegral n)]
  S.Selection expr selector operation -> do
    v <- evaluate ctx expr
    case operation of
      S.Get -> Right (get selector =<< v)
      S.Set ex ->
        evaluate ctx ex >>= asSingle <&> \x -> tmap selector (const x) <$> v
      S.PlusEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber <&> \x -> tmap selector (numOp (+ x)) <$> v
      S.MinusEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber <&> \x -> tmap selector (numOp (subtract x)) <$> v
      S.TimesEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber <&> \x -> tmap selector (numOp (* x)) <$> v
      S.DivEq ex ->
        evaluate ctx ex >>= asSingle >>= asNumber <&> \x -> tmap selector (numOp (/ x)) <$> v

asSingle :: [J.Value] -> Either String J.Value
asSingle [x] = Right x
asSingle xs = Left ("Expected one result, found " ++ show xs)

asNumber :: J.Value -> Either String Scientific
asNumber v = case v of
  J.Number n -> Right n
  _ -> Left ("Expected a number, found " ++ show v)

numOp :: (Scientific -> Scientific) -> J.Value -> J.Value
numOp f = \case
  J.Number n -> J.Number (f n)
  v -> v
