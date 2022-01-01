{-# LANGUAGE OverloadedStrings #-}

module Main where

import DG.Interpreter (Value (..), evaluate)
import DG.Parser (expression)
import qualified DG.Syntax as S
import qualified Data.Aeson as J
import qualified Data.Aeson.Parser as J
import qualified Data.Aeson.Text as J
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LT
import System.Environment (getArgs)
import Text.Megaparsec (eof, errorBundlePretty, parseErrorPretty, runParser)

main :: IO ()
main = do
  [rawProgram] <- getArgs
  case runParser (expression <* eof) "expression" (T.pack rawProgram) of
    Left errorBundle -> putStrLn (errorBundlePretty errorBundle)
    Right program -> do
      stdin <- LB.getContents
      case J.eitherDecode stdin of
        Left err -> putStrLn err
        Right v -> case evaluate (HM.fromList [(S.Identifier "x", JSON v)]) program of
          Left err -> putStrLn err
          Right values -> for_ values (LT.putStrLn . J.encodeToLazyText)
