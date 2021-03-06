{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (Exception (displayException))
import DG.Interpreter (evaluate, initialContext)
import DG.Json (compactPrint, deannotate, parseJson)
import DG.Parser (expression)
import DG.Runtime (Value (..))
import qualified DG.Syntax as DG
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (for_)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Streamly.Prelude as S
import System.Environment (getArgs)
import System.IO (stdin, stdout)
import Text.Megaparsec (eof, errorBundlePretty, runParser)

main :: IO ()
main = do
  [rawProgram] <- getArgs
  case runParser (expression <* eof) "expression" (T.pack rawProgram) of
    Left errorBundle -> putStrLn (errorBundlePretty errorBundle)
    Right program -> do
      parseJson stdin >>= \case
        Left err -> T.putStrLn err
        Right v -> case S.toList (evaluate (M.insert (DG.Identifier "x") (JSON v) initialContext) program) of
          Left err -> putStrLn (displayException err)
          Right values -> for_ values $ \case
            JSON r -> maybe (pure ()) (LB.hPut stdout . BB.toLazyByteString . compactPrint) (deannotate r) >> B.hPut stdout "\n"
            Function _ -> B.hPut stdout "<function>\n"
            Collector _ -> B.hPut stdout "<collector>\n"
