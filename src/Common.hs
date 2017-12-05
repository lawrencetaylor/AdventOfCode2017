module Common(
  readData
, Common.parse
) where

import System.FilePath
import System.Directory
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec as P

readData :: String -> IO String
readData relativePath =
  flip combine relativePath <$> getCurrentDirectory
  >>= readFile

parse :: Parser a -> String -> Either ParseError a
parse parser = P.parse parser []

