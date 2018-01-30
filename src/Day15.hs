module Day15 where

import Data.Bits

seedA = 591
seedB = 393

factorA = 16807
factorB = 48271

denominator = 2147483647

first16Bits = (.&.) (shiftL 1 16 - 1)

seq2 seed factor = fmap first16Bits $ iterate (next factor) seed

divisibleBy4 :: Int -> Bool
divisibleBy4 = (==) 0 . (.&.) 3

divisibleBy8 :: Int -> Bool
divisibleBy8 = (==) 0 .(.&.) 7

countMatching count [] = count
countMatching count ((h1,h2):t)
  | h1 == h2    = countMatching (count + 1) t
  | otherwise   = countMatching count t

next :: Int -> Int -> Int
next factor previous = 
  rem (factor * previous) denominator

partOne aStream bStream = countMatching 0 $ take 40000000 $ zip aS bS
  where
    aS = fmap first16Bits aStream
    bS = fmap first16Bits bStream

partTwo aStream bStream = countMatching 0 $ take 5000000 $ zip aS bS
  where
    aS = fmap first16Bits $ filter divisibleBy4 $ aStream
    bS = fmap first16Bits $ filter divisibleBy8 $ bStream

main :: IO ()
main = do
  let rawA = iterate (next factorA) seedA
  let rawB = iterate (next factorB) seedB
  let p1 = partOne rawA rawB
  let p2 = partTwo rawA rawB
  putStrLn $ "Day 15: (Part 1, Part 2) = " ++ (show (p1, p2))