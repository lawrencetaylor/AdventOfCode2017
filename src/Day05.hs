module Day05 where

import Common
import Data.Map as M
import Data.List as L

-- Solution

getInstructions :: [Int] -> Map Int Int
getInstructions = fromList . zip [0..]

nextPartOne :: Int -> Int
nextPartOne = succ

nextPartTwo :: Int -> Int
nextPartTwo n
  | n >= 3    = n-1
  | otherwise = n+1

solve :: [Int] -> (Int -> Int) -> Int
solve initial next = length $ unfoldr move (0, getInstructions initial)
  where
    move (currentPosition, instructions) = do
      jump <- M.lookup currentPosition instructions
      let newInstructions = M.insert currentPosition (next jump) instructions
      return $ ( (), (currentPosition +  jump, newInstructions))

main :: IO ()
main = do
  input <- fmap ((fmap read) . lines) $ readData "data\\Day05"
  let partOne = solve input nextPartOne
  let partTwo = solve input nextPartTwo
  putStrLn $ "Day 05: (Part 1, Part 2) = " ++ (show (partOne, partTwo))