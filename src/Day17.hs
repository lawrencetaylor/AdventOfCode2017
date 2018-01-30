module Day17 where

import Data.List(splitAt, foldl')

input :: Int
input = 344

positionsForLength :: Int -> [(Int, Int)]
positionsForLength n = scanl nextPos (0, 0) [1..n]
    where nextPos (_, currentPos) len = (len, ((currentPos + input) `mod` len) + 1)

insertAt :: [a] -> (a, Int) -> [a]
insertAt list (toInsert, index) = before ++ (toInsert : after)
  where 
    (before, after) = splitAt index list

part1 :: Int -> Int
part1 n = (!! (n+1)) . dropWhile (/= n) . foldl insertAt [] . positionsForLength $ n

part2 :: Int -> Int
part2 = foldl' go 0 . positionsForLength
    where go currentVal (l, current) = if current == 1 then l else currentVal
    
main :: IO ()
main = do
  let p1 = part1 2017
  let p2 = part2 50000000
  putStrLn $ "Day 17: (Part 1, Part 2) = " ++ (show (p1, p2))