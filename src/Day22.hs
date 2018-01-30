module Day22 where

import Common
import qualified Data.Map as M
import Data.Maybe(maybe)
import Data.List

-- Parsing

pMap :: String -> M.Map Position CellStatus
pMap input =
  M.fromList $ mconcat $ fmap f $ zip (reverse [-lim..lim] ) $ fmap z l
  where l@(h:t) = (fmap . fmap ) s $ lines input
        n = length h
        lim = quot (n-1) 2
        z r = zip [-lim..lim] r
        f (x, tl) = fmap (\(y, z) -> ((y, x), z)) tl
        s c = if c == '#' then Infected else Clean

data Direction = N | E | S | W
type Position = (Int, Int)
type Map = M.Map Position CellStatus
data Turn = R | L | F | B deriving (Show)

turn :: Turn -> Direction -> Direction
turn R N = E
turn R E = S
turn R S = W
turn R W = N
turn L N = W
turn L W = S
turn L S = E
turn L E = N
turn F d = d
turn B N = S
turn B S = N
turn B E = W
turn B W = E

step :: Position -> Direction -> Position
step (x, y) N = (x, y+1)
step (x, y) E = (x+1, y)
step (x, y) S = (x, y-1)
step (x, y) W = (x-1, y)

type Virus = (Position, Direction, Map)
type Infected = (Bool)
data CellStatus = 
  Clean
  | Weakened
  | Flagged
  | Infected
  deriving (Eq, Show)

transitionPartOne Clean = Infected
transitionPartOne Infected = Clean

transitionPartTwo Clean = Weakened
transitionPartTwo Weakened = Infected
transitionPartTwo Infected = Flagged
transitionPartTwo Flagged = Clean



move :: (CellStatus -> CellStatus) -> Virus -> Maybe (CellStatus, Virus)
move trans (pos, d, m) = Just (nowInfected, newVirus)
  where 
    currentInfected = maybe Clean id $ M.lookup pos m
    turnDir = 
      case currentInfected of
        Clean -> L
        Infected -> R
        Weakened -> F
        Flagged -> B
    d' = turn turnDir d
    nowInfected = trans currentInfected
    newPos = step pos d'
    newVirus = (newPos, d', M.insert pos nowInfected m)

partOne transition map n = 
  length $ 
  filter ((==) Infected) $ 
  take n $ 
  unfoldr (move transition) ((0,0), N, map)

main :: IO ()
main = do
  input <- readData "data\\Day22"
  let map = pMap input
  let p1 = partOne transitionPartOne map 10000
  let p2 = partOne transitionPartTwo map 10000000
  putStrLn $ "Day 22: (Part 1, Part 2) = " ++ (show (p1, p2))


    