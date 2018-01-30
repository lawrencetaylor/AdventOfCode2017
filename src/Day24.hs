module Day24 where

import Common
import Text.ParserCombinators.Parsec(Parser, many , digit, char)
import qualified Data.List as L
import Control.Monad.Trans.State
import Control.Applicative       (Alternative(empty, (<|>)))

-- Parsing

pPort :: Parser Int
pPort = read <$> many digit

pComponent :: Parser Component
pComponent = do
  first <- pPort
  char '/'
  second <- pPort
  return (first, second)

type Port = Int
type Component = (Int, Int)
type UsedComponent = (Int, Int)
type Strength = Int
type Bridge = [Component]
type Available = [Component]
type Length = Int

pickSingleElement :: [a] -> [(a,[a])]
pickSingleElement = go []
  where
   go _  [] = []
   go xs (y:ys) = (y,xs++ys) : go (y:xs) ys

b :: (Port, Length) -> StateT [Component] [] (Strength, Length)
b (port, length) = do
  (x,y) <- StateT pickSingleElement 
  next <- 
    case (x == port, y == port) of
      (True, _) -> return y
      (False, True) -> return x
      _ -> empty
  (after, l) <- (return (0, length)) <|> (b (next, length +1))
  return $ (x + y + after, l)

partOne components = 
  maximum $
  fmap fst $
  runStateT (b (0, 0)) components

partTwo components = 
  (fst. fst) $
  head $
  L.sortOn (\((s, l), _) -> (-l, -s)) $
  runStateT (b (0,0)) components

main :: IO ()
main = do
  input <- fmap lines $ readData "data\\Day24"
  let (Right components) = traverse (Common.parse pComponent) input 
  let p1 = partOne components
  let p2 = partTwo components
  putStrLn $ "Day 24: (Part 1, Part 2) = " ++ (show (p1, p2))
