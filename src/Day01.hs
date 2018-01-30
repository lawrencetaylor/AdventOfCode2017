{-# LANGUAGE FlexibleContexts #-}

module Day01 where

import Common
import Data.Char
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(many1, digit )
import Control.Monad.Reader
import Control.Monad.Trans.Either

-- Parsing

pDigits :: Parser [Int]
pDigits = (fmap . fmap) digitToInt $ many1 digit

sourceData :: EitherT ParseError IO [Int]
sourceData = EitherT $ fmap (parse pDigits) $ readData "data\\Day01"

-- Solution

getPairs :: Int -> [a] -> [(a,a)]
getPairs _ [] = []
getPairs shift l = zip l shiftedList
  where shiftedList = drop shift $ cycle l

equalToNext :: (Int,Int) -> Bool
equalToNext (a,b) = a == b

solve :: Int -> Reader [Int] Int
solve shift = reader $ (sum . fmap fst . filter equalToNext . getPairs shift)

main :: IO ()
main = do
  input <- runEitherT sourceData
  let partOne = fmap (runReader (solve 1)) input
  let partTwo = fmap (runReader (solve $ quot (length input) 2)) input
  putStrLn $ "Day 01: (Part 1, Part 2) = " ++ (show (partOne, partTwo))
