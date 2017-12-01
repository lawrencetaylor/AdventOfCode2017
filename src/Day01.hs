module Day01 where

import Data.Char
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(parse, many1, digit )
import System.FilePath
import System.Directory

-- Parsing

readData :: IO String
readData =
  flip combine "data\\Day01.txt" <$> getCurrentDirectory
  >>= readFile

pDigits :: Parser [Int]
pDigits = (fmap . fmap) digitToInt $ many1 digit

parseSourceData :: String -> Either ParseError [Int]
parseSourceData = parse pDigits []

getSourceData :: IO (Either ParseError [Int])
getSourceData = fmap parseSourceData readData

-- Solution
 
getPairs :: Int -> [a] -> [(a,a)]
getPairs _ [] = []
getPairs shift l = zip l shiftedList
  where shiftedList = drop shift $ cycle l

equalToNext :: (Int,Int) -> Bool
equalToNext (a,b) = a == b

getSum :: Int -> [Int] -> Int
getSum shift = sum . fmap fst . filter equalToNext . getPairs shift

solve :: [Int] -> (Int, Int)
solve input = (partOne, partTwo)
  where 
    partOne = getSum 1 input
    halfWay = quot ( length input ) 2
    partTwo = getSum halfWay input

main :: IO ()
main = do
  contents <- getSourceData
  let part1 = fmap solve contents
  putStrLn $ "(Part One, Part Two): " ++ (show part1)
  return ()