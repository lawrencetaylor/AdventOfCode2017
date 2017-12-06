module Day01 where

import Common
import Data.Char
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(many1, digit )

-- Parsing

pDigits :: Parser [Int]
pDigits = (fmap . fmap) digitToInt $ many1 digit

sourceData :: IO (Either ParseError [Int])
sourceData = fmap (parse pDigits) $ readData "data\\Day01"

-- Solution
 
getPairs :: Int -> [a] -> [(a,a)]
getPairs _ [] = []
getPairs shift l = zip l shiftedList
  where shiftedList = drop shift $ cycle l

equalToNext :: (Int,Int) -> Bool
equalToNext (a,b) = a == b

getSum :: Int -> [Int] -> Int
getSum shift = sum . fmap fst . filter equalToNext . getPairs shift

main :: IO ()
main = do
  contents <- sourceData
  let (Right input) = contents
  let partOne = getSum 1 input
  let partTwo = getSum (quot (length input) 2) input
  putStrLn $ "Day 01: (Part 1, Part 2) = " ++ (show (partOne, partTwo))
