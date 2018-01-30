module Day10(partTwo) where

import qualified Data.List.Split as S
import Data.Char
import Data.List
import qualified Data.Bits as B
import Numeric
import Text.Printf

inputPartOne :: [Int]
inputPartOne = [18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188]

inputPartTwo :: String
inputPartTwo = mconcat $ intersperse "," $ fmap show inputPartOne

type SkipSize = Int
type Current = Int
type Length = Int
type Knot = [Int]

data KnotState = Knot
  { skip :: Int
  , current :: Int
  , knot :: Knot }

seed :: KnotState
seed = Knot 0 0 [0..255]

reverseSegment :: Length -> [Int] ->[Int]
reverseSegment size = reverse . take size

unchangedSegment :: Length -> [Int] -> [Int]
unchangedSegment size knot = 
  take (length knot - size) $ drop size knot

iter :: KnotState -> Int -> KnotState
iter (Knot skip current k) len = Knot (skip+1) (mod (current + len + skip) n) e
  where 
    n = length k
    x = drop current (k ++ k)
    rev = reverse $ take len x
    rest = take (n - len) $ drop len x
    d = rev ++ rest ++ rev ++ rest
    e = take n $ drop (n - current) d

knotHash :: [Int] -> Knot
knotHash = knot . foldl iter seed

partOne :: [Int] -> Int
partOne input = 
  let 
    (h1:h2:_) = knotHash input
  in h1*h2

-- Part Two

toUnicodeInput :: [Char] -> [Int]
toUnicodeInput l = (fmap ord l) ++ [17, 31, 73, 47, 23]

partTwoHash :: [Int] -> KnotState
partTwoHash = (foldl iter seed) . mconcat . replicate 64

toHexString :: Int -> String
toHexString =  printf "%02x"

third :: (a, b, c) -> c
third (_,_,c) = c

partTwo :: String -> String
partTwo = 
  mconcat 
  . fmap (toHexString . foldl B.xor 0) 
  . S.chunksOf 16 
  . knot 
  . partTwoHash 
  . toUnicodeInput

main :: IO ()
main = do
  let p1 = partOne inputPartOne
  let p2 = partTwo inputPartTwo
  putStrLn $ "Day 10: (Part 1, Part 2) = " ++ (show (p1, p2))