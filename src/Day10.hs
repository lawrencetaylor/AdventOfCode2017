module Day10 where

import qualified Data.List.Split as S
import Data.Char
import qualified Data.Bits as B
import Numeric

inputPartOne :: [Int]
inputPartOne = [18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188]

inputPartTwo :: String
inputPartTwo = "18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188"

type SkipSize = Int
type Current = Int
type Length = Int

reverseSegment :: Length -> [Int] ->[Int]
reverseSegment size = reverse . take size

unchangedSegment :: Length -> [Int] -> [Int]
unchangedSegment size knot = 
  take (length knot - size) $ drop size knot

iter :: (SkipSize, Current, [Int]) -> Length -> (SkipSize, Current, [Int])
iter (skip, current, l) len = 
  let 
    n = length l
    newKnotUntruncated = (reverseSegment len l) ++ (unchangedSegment len l)
    newKnot = take n $ drop (len + skip) $ cycle newKnotUntruncated
    newCurrent = current + len + skip
  in (skip + 1, mod newCurrent n, newKnot)

rotateToCorrectOrder :: Current -> [Int] -> [Int]
rotateToCorrectOrder current l = take (length l) $ drop (length l - current) $ cycle l

knotHash :: [Int] -> (SkipSize, Current, [Int])
knotHash = foldl iter (0,0,[0..255])

partOne :: [Int] -> Int
partOne input = 
  let 
    (_, currentIndex, l) = knotHash input
    (h1:h2:_) = rotateToCorrectOrder currentIndex l
  in h1*h2

-- Part Two

toUnicodeInput :: [Char] -> [Int]
toUnicodeInput l = (fmap ord l) ++ [17, 31, 73, 47, 23]

partTwoHash :: [Int] -> (SkipSize, Current, [Int])
partTwoHash = knotHash . mconcat . replicate 64

toHexString :: Int -> String
toHexString = go . flip showHex ""
  where 
    go [a,b] = [a,b]
    go [a] = ['0', a]

third :: (a, b, c) -> c
third (_,_,c) = c

partTwo :: String -> String
partTwo = 
  mconcat 
  . fmap (toHexString . foldl B.xor 0) 
  . S.chunksOf 16 
  . third 
  . partTwoHash 
  . toUnicodeInput

main :: IO ()
main = do
  let p1 = partOne inputPartOne
  let p2 = partTwo inputPartTwo
  putStrLn $ "Day 10: (Part 1, Part 2) = " ++ (show (p1, p2))