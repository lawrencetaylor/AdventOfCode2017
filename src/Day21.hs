module Day21 where

import Common
import Text.ParserCombinators.Parsec(Parser)
import Text.Parsec
import Data.List.Split (chunksOf)
import Data.List
import qualified Data.Map as M
import Data.Map(Map, (!))

-- Parsing

pGrid :: Parser [String]
pGrid = sepBy ( many (char '.' <|> char '#') ) (char '/')

pRule :: Parser (Grid, Grid)
pRule = do
  first  <- pGrid
  string " => "
  second <- pGrid
  return (first, second)

-- Solution

type Grid = [[Char]]
type Rule = (Grid, Grid)
type Rules = Map Grid Grid

type Symmetry = Grid -> Grid

split :: Int -> Grid -> [[Grid]]
split n =  fmap chunkChunkedRows  .chunkRows
  where 
    chunkRows = chunksOf n
    chunkChunkedRows = fmap transpose . chunksOf n . transpose

reassemble :: [[Grid]] -> Grid
reassemble  = concat . fmap (fmap concat . transpose)

-- Rules are defined modulo action of symmetries of the square
-- { 1, r, rr, rrr, a, ar, arr, arrr}
-- r = rotation
-- a = flip

gR :: Symmetry
gR = reverse . transpose

gF :: Symmetry
gF = fmap reverse

d8 :: [Symmetry]
d8 = [ id, gR, gR.gR, gR.gR.gR
     , gF, gF.gR, gF.gR.gR, gF.gR.gR.gR ]

step :: Rules -> Grid -> Grid
step r g = reassemble $ (fmap . fmap) (\g' -> r ! g') $ split n g
  where
    n | length g `mod` 2 == 0 = 2
      | length g `mod` 3 == 0 = 3

expandRule :: (Grid, Grid) -> [(Grid, Grid)]
expandRule (gI, gO) = 
  fmap (\s -> (s gI, gO)) d8

seed :: Grid
seed = 
  [ ".#."
  , "..#"
  , "###" ]

numberSwitchedOn :: Rules -> Int -> Grid -> Int
numberSwitchedOn rules n = 
  length .
  filter ((==) '#') . 
  concat .
  last . take (n+1) . 
  iterate (step rules)

main :: IO ()
main = do
  input <- readData "data\\Day21"
  let (Right rules) = traverse (Common.parse pRule) $ lines $ input
  let completeRules = (M.fromList . mconcat) $ fmap expandRule rules 
  let p1 = numberSwitchedOn completeRules 5 seed
  let p2 = numberSwitchedOn completeRules 18 seed
  putStrLn $ "Day 21: (Part 1, Part 2) = " ++ (show (p1, p2))