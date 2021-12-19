{-# LANGUAGE LambdaCase #-}
module DG.Interpreter where

import qualified DG.Syntax as S
import qualified Data.Aeson as J
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

get :: S.Selector -> J.Value -> [J.Value]
get s v = case (s, v) of
  (S.Field (S.Identifier name), J.Object o) -> toList (o !? name)
  _ -> []

tmap :: S.Selector -> (J.Value -> J.Value) -> J.Value -> J.Value
tmap s f v = case (s, v) of
  (S.Field (S.Identifier name), J.Object o) ->
    J.Object (HM.update (pure . f) name o)
  _ -> v

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
      S.Set ex -> evaluate ctx ex >>= \case
        [x] -> Right (tmap selector (const x) <$> v)
        _ -> Left ("No results for " ++ show ex)
