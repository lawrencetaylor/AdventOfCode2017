module Day09 where

import Common

type IsGarbage = Bool
type Score = Int
type GarbageCount = Int

data State = State 
  { count :: Int
  , level :: Int
  , score :: Int
  , garbageCount :: Int }

m :: IsGarbage -> State -> String -> (Score, GarbageCount)
m False s []          = (score s, garbageCount s)
--Ignore
m False s ('!':_:t)   = m True  s t
m False s ('!':t)     = m False s t
-- Groups
m False s ('{':t)     = m False s { count = 1 + count s, level = 1 + level s} t
m False s ('}':t)     = m False s { score = score s + level s, level = level s - 1} t
-- Garbage
m False s ('<':t)     = m True s t
m True  s ('>':t)     = m False s t
m True  s (_:t)       = m True s { garbageCount = 1 + garbageCount s} t
--Other Characters
m False s (_:t)          = m False s t


main :: IO ()
main = do
  input <- readData "data\\Day09"
  let (p1, p2) = m False (State 0 0 0 0) input -- r (False, 0, 0, 0, 0) input
  putStrLn $ "Day 09: (Part 1, Part 2) = " ++ (show (p1, p2)) 