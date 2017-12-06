module Day06 where

import Data.Map as M
import Data.Set as S

type Position = Int
type Memory = Int
type Blocks = Map Position Memory

input :: [Memory]
input = [4 ,10 ,4 ,1 ,8 ,4 ,9 ,14 ,5 ,1 ,14 ,15 ,0 ,15 ,3 , 5]

state :: [Memory] -> Blocks
state = M.fromList . zip  [0..]

next :: Int -> Int
next n = mod (n+1) $ length input

indexToDistrubute :: Blocks -> Int
indexToDistrubute = fst . M.foldlWithKey go (-1, -1)
  where
    go (currentKey, currentMax) key value
      | value > currentMax   = (key, value)
      | otherwise            = (currentKey, currentMax)

deallocate :: Position -> Blocks -> (Blocks, Memory)
deallocate i blocks = (M.insert i 0 blocks, blocks ! i)

allocate :: Memory -> Position -> Blocks -> Blocks 
allocate 0 _ state = state
allocate n i state = allocate (n-1) (next i) newState
  where 
    iMem = state ! i
    newState = M.insert i (iMem + 1) state

redistribute :: Blocks -> Blocks
redistribute state = allocate m (next i) newState
  where
    i = indexToDistrubute state
    (newState, m) = deallocate i state

generateSeq :: Blocks -> [Blocks]
generateSeq = go S.empty
  where
    go seenStates thisState = 
      thisState : go ( S.insert thisState seenStates) (redistribute thisState)

firstDupe :: [Blocks] -> Int
firstDupe = go S.empty 0
  where 
    go seen count (h:t)  = 
      case S.member h seen of
        True -> count
        False -> go (S.insert h seen) (count+1) t

cycleDupe :: [Blocks] -> (Maybe Int)
cycleDupe = go M.empty 0
  where 
    go seen count (h:t)  = do
      case M.member h seen of
        True -> (fmap  ((-) count) $ M.lookup h seen)
        False -> go (M.insert h count seen) (count+1) t
      
main :: IO ()
main = do
  let partOne = firstDupe $ generateSeq (state input)
  let partTwo = cycleDupe $ generateSeq (state input)
  putStrLn $ "Day 06: (Part 1, Part 2) = " ++ (show (partOne, partTwo)) 