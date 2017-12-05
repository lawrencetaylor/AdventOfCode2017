module Day03 where

import Data.List as L
import Data.Map as M

type Position = (Int, Int)
type Cache = Map Position Int
type SpiralState = (Int, Cache)

input :: Int
input = 347991

spiralLevel :: Int -> Position -> Position
spiralLevel n (x,y)
  | x == n && y < n       = (x, y+1)
  | x == n && y == n      = (n-1, n)
  | y == n && x > -n      = (x-1, n)
  | y == n && x == -n     = (-n, n-1)
  | x == -n && y > -n     = (-n, y-1)
  | x == -n && y == -n    = (-n+1, -n)
  | y == -n && x < n      = (x+1, -n)
  | y == -n && x == -n    = (0,0)

spiral :: [Position]
spiral =  (0,0) : mconcat [ thisSpiral n | n <- [1..]]
  where thisSpiral n = take (8*n) $ iterate (spiralLevel n) (n, -n+1)

distanceAt :: Int -> Int
distanceAt x = distance $ spiral !! (x-1) 
  where distance (p, q) = abs p + abs q

getSum :: Cache -> Position -> Int
getSum m (x,y) = 
  sum $
    fmap value [
      (x+1, y)
    , (x+1, y+1)
    , (x, y+1)
    , (x-1, y+1)
    , (x-1, y)
    , (x-1, y-1)
    , (x, y-1)
    , (x+1, y-1) ] 
  where 
    value :: Position -> Int
    value p = maybe 0 id $ M.lookup p m
    
initialCache :: Cache
initialCache = M.fromList [((0,0), 1)]

generateSequence :: [Int]
generateSequence = L.unfoldr unfold (1, initialCache)
  where 
    unfold (index, cache) = Just (val, (index+1, newCache)) 
      where
        pos = spiral !! index
        val = getSum cache pos
        newCache = M.insert pos val cache

main :: IO ()
main = do
  let partOne = distanceAt input
  let partTwo = head $ L.filter ((<) input) generateSequence
  putStrLn $ "Day 03: (Part 1, Part 2) = " ++ (show (partOne, partTwo))