module Day08 where

import Common
import Text.ParserCombinators.Parsec(Parser)
import Text.Parsec as P
import Data.List as L
import Data.Map as M

-- Parsing

pRegister :: Parser Register
pRegister = manyTill anyChar $ char ' '

pNumber :: Parser Int
pNumber = fmap read $  manyTill anyChar (  pSpace <|> eof)
  where 
    pSpace = fmap (const ()) $ char ' '

pInc :: Parser (Int -> Int)
pInc = do
  string "inc "
  fmap (+) pNumber

pDec :: Parser (Int -> Int)
pDec = do
  string "dec "
  fmap (flip (-)) pNumber

pIf :: Parser String 
pIf = string "if "

operators :: [(String, Int -> Int -> Bool)]
operators = [    
    ("> ", flip (>))
  , (">= ", flip (>=))
  , ("== ", (==))
  , ("< ", flip (<))
  , ("<= ", flip (<=)) 
  , ("!= ", flip ((/=)))
  ]

pCondition :: Parser (Int -> Bool)
pCondition = do
  op <- choice $ fmap (try . string) (fmap fst operators)
  let (Just f) = L.lookup op operators
  n <- pNumber
  return $ f n

pInstruction :: Parser Instruction
pInstruction = do
  register <- pRegister
  instruction <- pInc <|> pDec
  pIf
  xRegister <- pRegister
  condition <- pCondition
  return (register, instruction, xRegister, condition)

-- Solution

type Register = String
type Condition = Int -> Bool
type Transform = Int -> Int
type Instruction = (Register, Transform, Register, Condition)

processLine :: Map String Int -> Instruction  -> Map String Int
processLine m (reg, instruction , testReg, condition) = 
  case condition testValue of
    True -> M.insert reg (instruction currentValue) m
    False -> m
  where
    testValue = maybe 0 id (M.lookup testReg m)
    currentValue = maybe 0 id (M.lookup reg m)
      
partOne :: [Instruction] -> [Map String Int]
partOne = return . (L.foldl processLine M.empty)

partTwo :: [Instruction] -> [Map String Int]
partTwo = L.scanl processLine M.empty

solve :: ([Instruction] -> [Map String Int]) -> [Instruction] -> Int
solve f = maximum . fmap snd . concat . fmap toList . f

main :: IO ()
main =  do
  rawString <- fmap (lines) $ readData "data\\Day08"
  let instructions = traverse (Common.parse pInstruction) rawString
  let p1 = fmap (solve partOne) instructions
  let p2 = fmap (solve partTwo) instructions
  putStrLn $ "Day 08: (Part 1, Part 2) = " ++ (show (p1, p2)) 