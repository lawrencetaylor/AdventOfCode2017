module Day14 where

import Numeric
import Data.Char
import qualified Day10 as D10
import qualified Data.Map as M
import Control.Monad.Loops

input = "ugkiagan"

pad :: String -> String
pad [a] = "000" ++ [a]
pad [a,b] = "00" ++ [a,b]
pad [a,b,c] = "0" ++ [a,b,c]
pad [a,b,c,d] = [a,b,c,d]

gridRow :: Char -> String
gridRow c = pad $ b
  where 
    [(i, _)] = readHex [c]
    b = showIntAtBase 2 intToDigit i "" 

row :: String -> Int -> String
row key i =  mconcat . fmap gridRow $ D10.partTwo (key ++ "-" ++ (show i))

rows :: String -> [String]
rows key = fmap (row key) [0..127]

countUsed :: [String] -> Int
countUsed l = length $ filter ((==) '1') $ mconcat l

index :: [a] -> [(Int, a)]
index l = zip [0..] l

rowsToMap :: [String] -> State
rowsToMap = M.fromList . mconcat . fmap flatten . (fmap . fmap) index . index 
  where 
    flatten (a, l) = fmap (\(b, c) -> ((a, b), c)) l

grid :: String -> M.Map Position Char
grid = rowsToMap . rows

type Position = (Int, Int)
type State = M.Map Position Char

checkAndMark :: ([Position], State) -> Maybe ([Position], State)
checkAndMark ([], _) = Nothing
checkAndMark ((h:t), m) = 
  case M.lookup h m of
    Just '1' -> Just (t ++ neighbours h, M.insert h '0' m) 
    _        -> Just (t, m)
  where 
    neighbours (x,y) = [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]

checkAndMark2 :: ([Position], State) -> State
checkAndMark2 ([], m) = m
checkAndMark2 ((h:t), m) =
  case M.lookup h m of 
    Nothing -> checkAndMark2 (t, m)
    Just '0' -> checkAndMark2 (t, m)
    Just '1' -> checkAndMark2 (t ++ neighbours h, M.insert h '0' m )
    Just c -> error $ "Unexpected character: " ++ show c
  where 
    neighbours (x,y) = [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]

x inp = inp : 
    case checkAndMark inp of
      Just y  -> x y
      Nothing -> []
y2 inp = snd $ last $ x inp
y5 inp = snd $ last $  unfoldM checkAndMark inp
 
nextUnmarked :: State -> Maybe Position
nextUnmarked l =  
  case filter ((/=) '0' . snd) $ M.toList l of
    [] -> Nothing
    ((h, _):_) -> Just h

findGroups :: Int -> State -> Int
findGroups count state = y
  where 
    y = 
      case nextUnmarked state of
      Nothing -> count
      Just p -> findGroups (count+1) $ checkAndMark2 ([p], state)

main :: IO ()
main = do
  let partOne = (countUsed . rows) input
  let partTwo = findGroups 0 $ grid input
  putStrLn $ "Day 08: (Part 1, Part 2) = " ++ (show (partOne, partTwo)) 