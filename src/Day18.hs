module Day18 where
  
import qualified Common as C
import Data.Map(Map)
import qualified Data.Map as M
import Data.Sequence(Seq(..), (!?))
import qualified Data.Sequence as S
import Text.Parsec(many, digit, char, letter, try, string, (<|>), choice, anyChar, sepBy, newline)
import Text.ParserCombinators.Parsec(Parser)
import Control.Monad.State
import Control.Monad.Loops

pNumber :: Parser Int
pNumber = read <$> many (digit <|> char '-')

pRegOrValue :: Parser RegOrValue
pRegOrValue =
  try (Left <$> letter)
  <|> try (Right <$> pNumber)

pSingle :: String -> Parser a -> Parser a
pSingle str p = do
  string str
  char ' '
  p

pRegister :: Parser Register
pRegister = letter

pPair :: String -> Parser a -> Parser b -> Parser (b, a)
pPair str pA pB = do
  string str
  char ' '
  first <- pB
  char ' '
  second <- pA
  return $ (first, second)
  
pInstruction :: Parser Instruction
pInstruction =
  choice
    [ (Snd) <$> try (pSingle "snd" pRegOrValue)
    , (uncurry Set) <$> try (pPair "set" pRegOrValue anyChar)
    , (uncurry Increase) <$> try (pPair "add" pRegOrValue anyChar)
    , (uncurry Multiply) <$> try (pPair "mul" pRegOrValue anyChar)
    , (uncurry Mod) <$> try (pPair "mod" pRegOrValue anyChar)
    , ((Rcv) <$> try (pSingle "rcv" anyChar))
    , (uncurry Jump) <$> try (pPair "jgz" pRegOrValue pRegOrValue)
    ]

pInstructions :: Parser (Seq Instruction)
pInstructions = fmap S.fromList $ sepBy pInstruction newline

-- Solution

type RegOrValue = Either Char Int
type Register = Char
type Registers = Map Register Int
type InstructionIndex = Int

data Instruction =
  Set Register RegOrValue
  | Increase Register RegOrValue
  | Multiply Register RegOrValue
  | Mod Register RegOrValue
  | Jump RegOrValue RegOrValue
  | Snd RegOrValue
  | Rcv Register
  deriving (Show, Eq)

data Machine = Machine
  { registers ::Registers
  , currentIndex :: InstructionIndex
  , input :: [Int]
  , output :: [Int]
  }
type Twins = (Machine, Machine)

published :: [Machine] -> Int
published [] = 0
published l = length . output $ last l 

stepTwinsS :: Seq Instruction -> State Twins (Maybe Int)
stepTwinsS instructions = do
  m0Pub <- firstTwin (unfoldM $ stepMachineS instructions)
  let m0PubCount = published m0Pub
  swapTwins
  m1Pub <- firstTwin (unfoldM $ stepMachineS instructions)
  let m1PubCount = published m1Pub
  swapTwins
  return $
    case (m0PubCount, m1PubCount) of
      (0,0) -> Nothing
      _     -> Just m1PubCount

firstTwin :: State Machine a -> State Twins a
firstTwin mS = state (\(m0, m1) -> 
  let (a, m0') = runState mS m0
  in (a, (m0', m1)))

swapTwins :: State Twins ()
swapTwins = state (\(m0, m1) -> ((), (m1 { input = output m0, output = []}, m0)))

stepMachineS :: Seq Instruction -> State Machine (Maybe Machine)
stepMachineS instructions = state $ (\m -> stepMachine instructions m)

stepMachine :: Seq Instruction -> Machine -> (Maybe Machine, Machine)
stepMachine instr m = 
  case instr !? (currentIndex m) of
  Nothing -> (Nothing, m)
  Just (Snd n) -> partOneSnd n m
  Just (Rcv n) -> partTwoRcv n m
  Just x -> let m' = step x m in (Just m', m')

step :: Instruction -> Machine -> Machine    
step (Set r rv) m =  moveForward 1 $ (insertR r $ valueOf rv m) m
step (Increase r rv) m = moveForward 1 $ (insertR r $ valueSum) m
  where
    valueSum = (valueOf (Left r) m) + (valueOf rv m)
step (Multiply r rv) m = moveForward 1 $ (insertR r $ valueProduct) m
  where
    valueProduct = (valueOf (Left r) m) * (valueOf rv m)
step (Mod r rv) m = moveForward 1 $ (insertR r $ valueMod) m
  where
    valueMod = (valueOf (Left r) m) `mod` (valueOf rv m)
step (Jump r rv) m =
  let testVal = valueOf r m
      skipVal = valueOf rv m
      skip = if testVal > 0 then skipVal else 1
  in moveForward skip m

type RcvHandler = Register -> Machine -> (Maybe Machine, Machine)
type SndHandler = RegOrValue -> Machine -> (Maybe Machine, Machine)

partTwoRcv :: RcvHandler
partTwoRcv r m =
  case input m of
    (h :t) -> 
      let m' = insertR r h m
          m'' = moveForward 1 $ m' { input = t}
      in (Just m'', m'')
    _       -> (Nothing, m)

partOneSnd :: SndHandler
partOneSnd r m =
  let m' = m { output = output m ++ [valueOf r m]
              , currentIndex = 1 + currentIndex m }
  in (Just m', m')

moveForward :: Int -> Machine -> Machine
moveForward n ms = ms { currentIndex = n + currentIndex ms}

valueOf :: RegOrValue -> Machine -> Int
valueOf (Left r) m = maybe 0 id $ M.lookup r $ registers m
valueOf (Right    v) _ = v

insertR :: Register -> Int -> Machine -> Machine
insertR r i ms = ms { registers = newRegs }
  where newRegs = M.insert r i (registers ms)

main :: IO()
main = do
  (Right instr) <- fmap (C.parse pInstructions ) $ C.readData "data\\Day18"
  let m0 = Machine (M.fromList $ [('p', 0)]) 0 [] []
  let m1 = Machine (M.fromList $ [('p', 1)]) 0 [] []
  let p1 = (last . output . snd) $ runState (unfoldM $ stepMachineS instr) m0
  let p2 = (sum . fst) $ runState(unfoldM $ stepTwinsS instr) (m0, m1)
  putStrLn $ "Day 17: (Part 1, Part 2) = " ++ (show (p1, p2))
