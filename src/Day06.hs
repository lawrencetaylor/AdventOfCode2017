{-# LANGUAGE TupleSections #-}

module Day06 where

import Data.Map(Map, (!))
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Loops

type Position = Int
type Memory = Int
type Blocks = Map Position Memory

hoist :: (s -> a) -> State s a
hoist f = state $ (\s -> (f s, s))

input :: [Memory]
input = [4 ,10 ,4 ,1 ,8 ,4 ,9 ,14 ,5 ,1 ,14 ,15 ,0 ,15 ,3 , 5]

next :: Int -> Int
next n = mod (n+1) $ length input

indexToDistrubuteS :: State Blocks Int
indexToDistrubuteS = state $ (\blocks -> 
  (fst . M.foldlWithKey go (-1, -1) $ blocks, blocks))
  where 
    go (currentKey, currentMax) key value
      | value > currentMax   = (key, value)
      | otherwise            = (currentKey, currentMax)

deallocateS :: Position -> State Blocks Memory
deallocateS i = state $ (\blocks -> 
  (blocks ! i, M.insert i 0 $ blocks))

allocateF :: Memory -> (Position, Blocks) -> (Position, Blocks)
allocateF _ (i, blocks) = (next i, M.insert i iMem blocks)
  where
    iMem = (blocks ! i) + 1

allocateS :: Memory -> Position -> State Blocks Blocks
allocateS n i = state $ (\blocks -> 
  let 
      nextIndex = mod (i+1) (length input)
      newBlocks = snd $ Prelude.foldr allocateF (nextIndex, blocks) [1..n] 
  in  (blocks , newBlocks)
  )

toIntList :: Blocks -> [Int]
toIntList = (fmap snd) . M.toList

redistributeS :: State Blocks Blocks
redistributeS = do
  orignalBlocks <- hoist id
  i <- indexToDistrubuteS
  newMemory <- deallocateS i
  allocateS newMemory i
  return orignalBlocks

blockSeqS :: State Blocks [Blocks]
blockSeqS = unfoldM $ fmap Just redistributeS

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

seed :: Blocks
seed = (M.fromList . zip  [0..]) input

solve :: ([Blocks] -> a) -> Blocks -> a
solve f seed = fst $ runState (fmap f blockSeqS) seed

main :: IO ()
main = do
  let partOne =  solve firstDupe seed
  let partTwo = solve cycleDupe seed
  putStrLn $ "Day 06: (Part 1, Part 2) = " ++ (show (partOne, partTwo)) 