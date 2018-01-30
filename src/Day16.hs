module Day16 where

import Common
import Text.ParserCombinators.Parsec(Parser)
import Text.Parsec

-- Parsing

pNumber :: Parser Int
pNumber = read <$> many digit

pSpin :: Parser Move
pSpin = do
  char 's'
  Spin <$> pNumber

pExchange :: Parser Move
pExchange = do
  char 'x'
  first <- pNumber
  char '/'
  second <- pNumber
  return $ Exchange first second

pPartner :: Parser Move
pPartner = do
  char 'p'
  first <- anyChar
  char '/'
  second <- anyChar
  return $ Partner first second

pMove :: Parser Move
pMove = 
  try pSpin
  <|> try pExchange
  <|> try pPartner

pMoves :: Parser [Move]
pMoves = sepBy pMove (char ',')

-- Solution

data Move = 
  Spin Int
  | Exchange Int Int
  | Partner Char Char deriving (Show)

type Position = Int
type Program = Char
type Programs = [Char]

spin :: Int -> Programs -> Programs
spin count p = 
  take (length p) $ drop (length p - count) $ p ++ p

exchange :: Int -> Int -> Programs -> Programs
exchange i j l = fmap go (zip l [0..])
  where 
    go :: (Char, Int) -> Char
    go ( x, k )
      | k == i   = l !! j
      | k == j   = l !! i
      | otherwise = x

partner :: Char -> Char -> Programs -> Programs
partner a b =  fmap go
  where 
    go c
      | a == c    = b
      | b == c    = a
      | otherwise = c

move :: Move -> Programs -> Programs
move (Spin count) = spin count
move (Exchange i j) = exchange i j
move (Partner a b) = partner a b

dance ::Programs -> [Move] -> Programs
dance = foldl (flip move)

danceSequence :: [Move] -> Programs -> [Programs]
danceSequence moves programs = programs : (danceSequence moves programs)

cycleLength :: [Move] -> [Programs] -> Programs -> Programs
cycleLength moves seen program = 
  case elem program seen of
    False -> 
      cycleLength moves (program : seen) (dance program moves)
    True  -> (reverse seen) !! 16

main :: IO ()
main  = do
  let input = Common.readData "Data\\Day16"
  (Right moves) <- fmap (Common.parse pMoves) input
  let initial = ['a'..'p']
  let p1 = dance initial $ moves
  let p2Prec = cycleLength moves [] initial
  putStrLn $ "Day 16: (Part 1, Part 2) = " ++ (show (p1, p2Prec))