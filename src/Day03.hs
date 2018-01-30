module Day03 where

import Data.List(drop)
import Data.Map(Map)
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Loops

type Level = Int
type Position = (Int, Int)
data Direction = N | E | S | W deriving (Show)

data SpiralState = Spiral 
  { position :: Position
  , direction :: Direction 
  , level :: Level
  , visited :: Map Position Int }
  deriving (Show)

next :: SpiralState -> SpiralState
next s@(Spiral (_, y) N n _) 
  | y == n      = s { position = (n-1, n), direction = W }
  | otherwise   = s { position = (n, y+1) }
  
next s@(Spiral (x, _) W n _)
  | x == -n      = s { position = (-n, n-1), direction = S }
  | otherwise    = s { position = (x-1,  n) }

next s@(Spiral (_, y) S n _) 
  | y == -n      = s { position = (-n+1, -n), direction = E}
  | otherwise    = s { position = (-n,  y-1) }

next s@(Spiral (x, _) E n _) 
  | x == n       = s { position = (n+1, -n), direction = N, level = n+1 }
  | otherwise    = s { position = (x+1, -n) }

distance :: (Int, Int) -> Int
distance (p, q) = abs p + abs q

seed :: SpiralState
seed = Spiral (0,0) E 0 (M.fromList [((0,0), 1)])

partOneState :: State SpiralState Int
partOneState = state $ (\s -> (distance $ position s, next s))

partTwoState :: State SpiralState Int
partTwoState = state (\s -> 
  let
    (x,y) = position s
    c = visited s
    value p = maybe 0 id $ M.lookup p c
    xCoords = (+) <$> [x] <*> [1,0,-1]
    yCoords = (+) <$> [y] <*> [1,0,-1]
    positions = (,) <$> xCoords <*> yCoords
    sValue = sum $ fmap value positions
    newVisited = M.insert (x,y) sValue c
  in 
    (sValue, (next s) { visited = newVisited}))

spiral :: State SpiralState Int -> State SpiralState [Int]
spiral = unfoldM . fmap Just

main :: IO ()
main = do
  let input = 347991
  let (partOne, _) = runState (fmap (head . drop (input - 1)) $ spiral partOneState) seed
  let (partTwo, _) = runState (fmap (head . dropWhile ((>) input)) $ spiral partTwoState) seed
  putStrLn $ "Day 03: (Part 1, Part 2) = " ++ (show (partOne, partTwo))