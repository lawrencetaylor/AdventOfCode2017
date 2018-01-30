module Day19 where

import Common 
import Data.List
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Char

-- Parsing

pRow :: String -> [(Int, Char)]
pRow = zip [0..]

pMaze :: String -> Maze
pMaze = M.fromList . filter removeEmpty . mconcat . fmap rejig . zip [0..] . fmap pRow . lines
  where rejig (j, r) = [((i,j), c) | (i, c) <- r]
        removeEmpty (_, ' ') = False
        removeEmpty _ = True

-- Solution

data Direction = N | E | S | W deriving (Show)
type Position = (Int, Int)
type Maze = Map Position Char
type RouteState = (Position, Direction)

next :: Position -> Direction -> Position
next (x, y) S = (x, y+1)
next (x, y) N = (x, y-1)
next (x, y) E = (x+1, y)
next (x, y) W = (x-1, y)

testDirs :: Direction -> [Direction]
testDirs N = [E,W]
testDirs S = [E,W]
testDirs E = [N,S]
testDirs W = [N,S]

move :: Maze -> RouteState -> Maybe (Maybe Char, RouteState)
move maze (pos, dir )
  | M.lookup (next pos dir) maze == Just '|' = Just (Nothing, (next pos dir, dir))
  | M.lookup (next pos dir) maze == Just '-' = Just (Nothing, (next pos dir, dir))
  | M.lookup (next pos dir) maze == Just '+' = Just (Nothing, (next pos dir, d))
  | (fmap isLetter $ M.lookup (next pos dir) maze) == Just True = 
    Just (M.lookup (next pos dir) maze, (next pos dir, dir))
    where 
      nextPos = next pos dir
      [d] = filter (\d' ->  isJust $ M.lookup (next nextPos d') maze) $ testDirs dir
move _ _ = Nothing

main :: IO ()
main = do
  input <- readData "data\\Day19"
  let mazeSeq = unfoldr (move $ pMaze input) ((1, 0), S)
  let p1 = concat $ traverse id $  filter isJust $ mazeSeq
  let p2 = 1 + (length $ mazeSeq)
  putStrLn $ "Day 17: (Part 1, Part 2) = " ++ (show (p1, p2))