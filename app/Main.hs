{-# LANGUAGE OverloadedStrings #-}

module Main where

import DG.Interpreter (evaluate, initialContext)
import DG.Parser (expression)
import DG.Runtime (Value (..))
import qualified DG.Syntax as S
import qualified Data.Aeson as J
import qualified Data.Aeson.Text as J
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (for_)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LT
import System.Environment (getArgs)
import Text.Megaparsec (eof, errorBundlePretty, runParser)

main :: IO ()
main = do
  [rawProgram] <- getArgs
  case runParser (expression <* eof) "expression" (T.pack rawProgram) of
    Left errorBundle -> putStrLn (errorBundlePretty errorBundle)
    Right program -> do
      stdin <- LB.getContents
      case J.eitherDecode stdin of
        Left err -> putStrLn err
        Right v -> case evaluate (M.insert (S.Identifier "x") (JSON v) initialContext) program of
          Left err -> putStrLn err
          Right values -> for_ values (LT.putStrLn . J.encodeToLazyText)
