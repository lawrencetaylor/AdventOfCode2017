module Day07 where

import Common
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec as P
import Data.List as L
import Data.Tree as T
import Control.Monad.Trans.Either

-- Parsing

pEnd :: Parser ()
pEnd = pSpace <|> pEoL <|> pEoF
  where 
  pSpace = fmap (const ()) $ char ' '
  pEoL = fmap (const ()) endOfLine
  pEoF = eof

pProgram :: Parser String
pProgram = many1 $ satisfy (\c -> c /= ' ' && c /= ',')

pWeight :: Parser Int
pWeight = do
  fmap read $ between (char '(') (char ')') (many digit)

pArrow :: Parser ()
pArrow = do
  string "->"
  return ()

pDependants :: Parser [String]
pDependants = do
  char ' '
  pArrow
  char ' '
  sepBy pProgram $ P.string ", "
  
pLine :: Parser ([String], (String, Int))
pLine = do
  parent <- pProgram
  char ' '
  weight <- pWeight
  maybeDependants <- P.optionMaybe pDependants
  let parents = maybe [] id maybeDependants
  return $ (parents, (parent, weight))

parseNodes :: String -> Either ParseError [Node]
parseNodes = traverse (Common.parse pLine) . lines

-- Solution

type Program = String
type Weight = Int
type WeightSum = Int
type Node = ([Program], (Program, Weight))

isDependant :: Program -> Node -> Bool
isDependant p = elem p . fst

dependencies :: Program -> [Node] -> [Program]
dependencies p = fmap (fst . snd) . filter  (isDependant p)

dependants :: Program -> [Node] -> ([Program], Weight)
dependants p n = (d, w)
  where 
    [(d, (_, w))] = filter ( (==) p . fst . snd ) n

getRoot :: Program -> [Node] -> Program
getRoot p nodes = 
  case dependencies p nodes of
    [] -> p
    (h:_) -> getRoot h nodes

toTree :: [Node] -> Program -> Tree (Program, Weight)
toTree n p = 
  let (d, w) = dependants p n 
  in
    Node {
      rootLabel = (p, w)
    , subForest = fmap (toTree n) d
    } 

toWeightTree :: Tree (Program, Weight) -> Tree (Program, Weight, WeightSum)
toWeightTree n@(Node (p, w) f) = 
  Node 
    (p, w, foldr (+) 0 $ fmap snd n)
    (fmap toWeightTree f)

getDifferent :: [WeightSum] -> Maybe (WeightSum, WeightSum)
getDifferent l = y
  where
    z = sortOn fst $ (fmap . fmap) (\x -> (length x, head x)) group $ sort l
    y = case z of
      [] -> Nothing
      [_] -> Nothing
      [(_, single), (_, rest)] -> Just (single, rest)


wrongUnFixed :: Tree (Program, Weight, WeightSum) -> (Program, Int)
wrongUnFixed = go 0
  where
    go toAdd (Node l f) = y
      where
        program (p, _, _) = p
        weightSum (_,_,ws) = ws
        weight (_, w, _) = w
        x =  fmap (weightSum . rootLabel) f
        y = case getDifferent x of
          Nothing -> (program l, toAdd + weight l)
          Just (diff, rest) -> go (rest - diff) root 
            where
              [root] = filter ((==) diff . weightSum . rootLabel) f
  
main :: IO ()
main = do
  let nodes = EitherT . fmap parseNodes $ readData "data\\Day07"
  let aNode = fmap (fst . snd . head) nodes
  let partOneE =  getRoot <$> aNode <*> nodes
  let tree = toTree <$> nodes <*> partOneE
  let partTwoE = fmap (snd . wrongUnFixed . toWeightTree) tree
  (Right partOne) <- runEitherT partOneE
  (Right partTwo) <- runEitherT partTwoE
  putStrLn $ "Day 05: (Part 1, Part 2) = " ++ (show (partOne, partTwo)) 
  return ()