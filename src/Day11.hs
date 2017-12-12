module Day11 where

import Common
import Text.ParserCombinators.Parsec(Parser)
import Text.Parsec as P

data Direction = N | NE | SE | S | SW | NW deriving (Show)

-- Parsing

strDirMap :: [([Char], Direction)]
strDirMap = [
    ("ne", NE)
  , ("nw", NW)
  , ("n", N)
  , ("se", SE)
  , ("sw", SW)
  , ("s", S)  ]

pDirectionCase :: (String , Direction) -> Parser Direction
pDirectionCase (dStr, d) = do
  string dStr
  return d

pDirection :: Parser Direction 
pDirection = choice $ fmap (try . pDirectionCase) strDirMap

pDirections :: Parser [Direction]
pDirections = sepBy pDirection (char ',')

-- Solution

steps :: (Int, Int) -> Direction -> (Int, Int)
steps (x, y) N = (x, y+2)
steps (x, y) NE = (x+1, y+1)
steps (x, y) SE = (x+1, y-1)
steps (x, y) S  = (x, y-2)
steps (x, y) SW = (x-1, y-1)
steps (x, y) NW = (x-1, y+1)

stepBack :: Int ->  (Int, Int) -> Int
stepBack count (x, y)
  | x == 0 && y == 0  = count
  | x == 0 && y < 0   = stepBack (count+1) (x, y+2)
  | x == 0 && y > 0   = stepBack (count+1) (x, y-2)
  | x >  0 && y == 0  = stepBack (count+1) (x-2, y)
  | x <  0 && y == 0  = stepBack (count+1) (x+2, y)

  | x < 0  && y < 0 = stepBack (count+1) (x+1, y+1)
  | x < 0  && y > 0 = stepBack (count+1) (x+1, y-1)
  | x > 0  && y < 0 = stepBack (count+1) (x-1, y+1)
  | x > 0  && y > 0 = stepBack (count+1) (x-1, y-1)

main :: IO ()
main = do
  input <- readData "data\\Day11"
  let directions = fmap (scanl steps (0,0)) $ Common.parse pDirections input
  let p1 = fmap (stepBack 0 . last) $ directions
  let p2 = fmap maximum $ (fmap . fmap) (stepBack 0) $ directions
  putStrLn $ "Day 11: (Part 1, Part 2) = " ++ (show (p1, p2)) 



