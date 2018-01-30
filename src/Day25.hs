module Day25 where

import Data.Maybe(maybe)
import Data.Map(Map, (!?), insert, empty, toList)
import qualified Data.Map as M

type Tape = Map Int Int

data State = A | B | C | D | E | F
  deriving (Show)

data TuringState = Turing {
  state :: State
, position :: Int
, tape :: Map Int Int
} deriving (Show)

value :: TuringState -> Int
value (Turing _ p t) =  maybe 0 id $ t !? p

write :: Int -> TuringState -> TuringState
write n ts@Turing { position = p, tape = t} = 
  ts { tape = insert p n t}

shift :: Int -> TuringState -> TuringState
shift n ts@Turing { position = p} = ts { position = p + n}

left = shift (-1)
right = shift (1)

setState :: State -> TuringState -> TuringState
setState s ts = ts { state = s}

move :: TuringState -> TuringState
move ts@Turing { state = A} = 
  case value ts of
  0 -> (setState B . right . write 1) ts
  1 -> (setState E . left . write 1) ts

move ts@Turing { state = B} = 
  case value ts of
  0 -> (setState C . right . write 1) ts
  1 -> (setState F . right . write 1) ts

move ts@Turing { state = C} = 
  case value ts of
  0 -> (setState D . left . write 1) ts
  1 -> (setState B . right . write 0) ts

move ts@Turing { state = D} = 
  case value ts of
  0 -> (setState E . right . write 1) ts
  1 -> (setState C . left . write 0) ts
  
move ts@Turing { state = E} = 
  case value ts of
  0 -> (setState A . left . write 1) ts
  1 -> (setState D . right . write 0) ts

move ts@Turing { state = F} = 
  case value ts of
  0 -> (setState A . right . write 1) ts
  1 -> (setState C . right . write 1) ts

move2 ts@Turing { state = A} = 
  case value ts of
  0 -> (setState B . right . write 1) ts
  1 -> (setState B . left  . write 0) ts

move2 ts@Turing { state = B} = 
  case value ts of
  0 -> (setState A . left . write 1) ts
  1 -> (setState A . right. write 1) ts

iterations = 12459852

seed :: TuringState
seed = Turing { state = A, position = 0, tape = empty}

checkSum :: TuringState -> Int
checkSum (Turing _ _  t) = 
  length . toList $ M.filter ((==) 1) t

main :: IO ()
main = do
  let p1 = checkSum $ last $ take (iterations + 1)$ iterate move seed
  putStrLn $ "Day 24: (Part 1, Part 2) = " ++ (show (p1, "--"))
