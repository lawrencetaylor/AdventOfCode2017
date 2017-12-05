module Day04 where

import Common
import Data.List as L

-- Solution

areUnique :: (Eq a) => [a] -> Bool
areUnique items = rawLength == dedupedLength
  where 
    rawLength = length items
    dedupedLength = length $ nub items

solve :: Eq a => (String -> [a]) -> String -> Int
solve f str = length $ L.filter (areUnique . f) $ lines str

main :: IO ()
main = do
  input <- readData "data\\Day04.txt"
  let partOne = solve words input
  let partTwo = solve (fmap sort . words) input
  putStrLn $ "Day 04: (Part 1, Part 2) = " ++ (show (partOne, partTwo))