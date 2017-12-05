module Day04 where

import Common
import Data.List as L
import Data.Map as M

-- Solution

areUnique :: (Eq a) => [a] -> Bool
areUnique items = rawLength == dedupedLength
  where 
    rawLength = length items
    dedupedLength = length $ nub items

toCharMap :: String -> Map Char Int
toCharMap = 
  M.fromList
  . fmap (\x -> (head x, length x))
  . group
  . sort

solve :: Eq a => (String -> [a]) -> String -> Int
solve f str = length $ L.filter (areUnique . f) $ lines str

main :: IO ()
main = do
  input <- readData "data\\Day04.txt"
  let partOne = solve words input
  let partTwo = solve (fmap toCharMap . words) input
  putStrLn $ "Day 04: (Part 1, Part 2) = " ++ (show (partOne, partTwo))