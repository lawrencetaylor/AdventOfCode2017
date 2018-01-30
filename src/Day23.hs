module Day23 where

import Common
import Text.ParserCombinators.Parsec(Parser, try, (<|>), sepBy, newline)
import qualified Data.Map as M
import Data.List(intersperse)
import Data.Sequence(Seq(..), (!?), fromList)
import Day18(pPair, pRegOrValue, pRegister, Registers, Register, RegOrValue)
import Control.Monad.Trans.State
import Control.Monad.Loops

-- Parsing

pInstruction :: Parser Instruction
pInstruction = 
  (uncurry Set) <$> try (pPair "set" pRegOrValue pRegister)
  <|> (uncurry Sub) <$> try (pPair "sub" pRegOrValue pRegister)
  <|> (uncurry Mul) <$> try (pPair "mul" pRegOrValue pRegister)
  <|> (uncurry Jnz) <$> try (pPair "jnz" pRegOrValue pRegOrValue)

pInstructions :: Parser (Seq Instruction)
pInstructions = fmap fromList $ sepBy pInstruction newline

-- Solution

data Instruction = 
  Set Register RegOrValue
  | Sub Register RegOrValue
  | Mul Register RegOrValue
  | Jnz RegOrValue RegOrValue
  deriving (Show)

data Machine = Machine {
  registers :: Registers
, currentIndex :: Int
} 

instance Show (Machine) where
  show m = show (currentIndex m, (mconcat . intersperse "," . fmap (show) . M.toList . registers) m)


seed :: Machine
seed = Machine { registers = M.empty, currentIndex = 0}

moveForward :: Int -> StateT Machine IO ()
moveForward n = state (\m -> 
  ((), m { currentIndex = n +  currentIndex m}))

insert :: Register -> Int -> StateT Machine IO ()
insert r v = state (\m -> 
  ((), m { registers = M.insert r v (registers m) }))

value :: RegOrValue -> StateT Machine IO Int
value (Right v) = return v
value (Left r) = state (\m -> 
  (maybe 0 id . M.lookup r $ registers m, m))

currentState :: StateT Machine IO Machine 
currentState = state $ (\m -> (m, m))

waitIO :: StateT t IO String
waitIO = StateT (\m -> 
  do
    x <- getLine
    return (x, m))

printIO :: Show a => a -> StateT t IO ()
printIO a = StateT (\m -> 
  do
    print a
    return ((), m))



step :: Instruction -> StateT Machine IO ()
step (Set r rv) =  value rv >>= insert r >>= (\_ -> moveForward 1)
step (Sub r rv)  = do
  rVal <- value (Left r)
  vVal <- value rv
  insert r (rVal -  vVal)
  moveForward 1
step (Mul r rv) = do
  rVal <- value (Left r)
  vVal <- value rv
  insert r (rVal * vVal)
  moveForward 1
step (Jnz r rv) = do
  rVal <- value r
  vVal <- value rv
  if rVal /= 0 then 
    moveForward vVal
  else moveForward 1

currentInstruction :: Seq Instruction -> StateT Machine IO (Maybe Instruction)
currentInstruction instructions = state $ (\m -> 
  (instructions !? currentIndex m, m))

isMul :: Instruction -> Bool
isMul (Mul _ _) = True
isMul _ = False

stepMachine :: Seq Instruction -> StateT Machine IO (Maybe Instruction)
stepMachine instructions = do
  instruction <- currentInstruction instructions 
  _ <-  case instruction of
        Nothing -> return ()
        Just i -> step i
  return $ instruction

factors :: Int -> [Int]
factors n = [x | x <- [1..sn], mod n x == 0]
    where sn =  (truncate . sqrt . fromIntegral $ n)

isComposite :: Int -> Bool
isComposite = (<) 1 . length . factors

-- See Day23.1 /2 /3 in data
partTwo :: Int
partTwo = 
  let start = 57*100 + 100000
      end = start + 17000
  in length $ filter isComposite [start, start + 17 ..end]

main :: IO ()
main = do
  (Right instructions) <- fmap (Common.parse pInstructions) $ Common.readData "data\\Day23"
  let p1State = unfoldM $ stepMachine instructions
  p1 <- fmap (fst . last . zip [1..] . filter isMul . fst) $ runStateT p1State seed
  putStrLn $ "Day 23: (Part 1, Part 2) = " ++ (show (p1, "p2"))
  print "End" 
