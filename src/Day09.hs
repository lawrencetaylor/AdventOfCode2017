module Day09 where

import Common

type IsGarbage = Bool
type Count = Int
type Level = Int
type Score = Int
type GarbageCount = Int

r :: (IsGarbage, Count, Level, Score, GarbageCount) -> String -> (Score, GarbageCount)
r (False, count, level, score, gCount) [] = (score, gCount)
-- Ignore
r (True, count, level, score, gCount) ('!':_:t) = r (True, count, level, score, gCount) t
r (False, count, level, score, gCount) ('!':t) = r (False, count, level, score, gCount) t
-- Groups
r (False, count, level, score, gCount) ('{':t) = r (False, count + 1, level + 1, score, gCount) t
r (False, count, level, score, gCount) ('}':t) = r (False, count, level-1, score+level, gCount) t
-- Garbage
r (False, count, level, score, gCount) ('<':t) = r(True, count, level, score, gCount) t
r (True, count, level, score, gCount) ('>':t) = r (False, count, level, score, gCount) t
r (True, count, level, score, gCount) ( _:t) = r (True, count, level, score, gCount+1) t
-- Other characters
r (False, count, level, score, gCount) (',':t) = r(False, count, level, score, gCount) t

main :: IO ()
main = do
  input <- readData "data\\Day09"
  let (p1, p2) = r (False, 0, 0, 0, 0) input
  putStrLn $ "Day 09: (Part 1, Part 2) = " ++ (show (p1, p2)) 