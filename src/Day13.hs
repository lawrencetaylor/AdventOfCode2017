module Day13 where

import Common
import Text.ParserCombinators.Parsec(Parser)
import Text.Parsec
import Data.Maybe

-- Parsing

pDepth :: Parser Depth
pDepth = read <$> manyTill digit (char ':')

pRange :: Parser Range
pRange = read <$> many digit

pWall :: Parser (Depth, Range)
pWall = do
  pos <- pDepth
  char ' '
  ran <- pRange
  return (pos, ran)

-- Solution

type Range = Int
type Depth = Int
type Severity = Int
type Delay = Int

caught :: Delay -> (Depth, Range) -> Bool
caught delay (depth, range) = (delay+depth) `mod` (2*range - 2) == 0

severity :: Delay -> (Depth, Range) -> Maybe Severity
severity delay (depth, range) = 
  case caught delay (depth, range) of
    True -> Just $ depth * range
    False -> Nothing

sumMaybe :: Maybe Severity -> Maybe Severity -> Maybe Severity
sumMaybe Nothing Nothing = Nothing
sumMaybe Nothing (Just b) = Just b
sumMaybe (Just a) Nothing = Just a
sumMaybe (Just a) (Just b) = Just (a+b)

main :: IO ()
main = do
  (Right input) <- (traverse (Common.parse pWall) . lines) <$> readData "data\\Day13"
  let p1 = fmap sum $ sequence $ filter isJust $ fmap (severity 0) input
  let delaySeverity n = fmap sum $ sequence $ filter isJust $ fmap (severity n) input
  let p2 = head $ dropWhile (isJust . snd) $ zip [0..] $ fmap delaySeverity [0..]
  putStrLn $ "Day 13: (Part 1, Part 2) = " ++ (show (p1, p2))