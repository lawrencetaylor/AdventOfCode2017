module Day12 where

import Common
import Text.ParserCombinators.Parsec(Parser)
import Text.Parsec
import  Data.Set(Set)
import qualified Data.Set as S

  -- Parsing

pProgram :: Parser Int
pProgram = read <$> manyTill digit (const () <$> char ',' <|> const () <$> char ' ' <|> eof)

pRelationship :: Parser (Int, Set Int)
pRelationship = do
  root <- pProgram
  string "<-> "
  children <- sepBy pProgram (char ' ')
  return (root, S.fromList children)

-- Solution

getConnected :: [(Int, Set Int)] -> Set Int -> [Int] -> Set Int
getConnected _ connected [] = connected 
getConnected mappings seen (root : t) = 
  getConnected mappings (S.insert root seen) (t ++ notSeenYet)
  where
    (Just children) = lookup root mappings
    notSeenYet = S.toList $ S.difference children seen

getConnectedTo :: Int -> [(Int, Set Int)] -> Set Int
getConnectedTo p mappings = getConnected mappings S.empty [p]

aggGroups :: [(Int, Set Int)] -> [Set Int] -> Int -> [Set Int]
aggGroups mappings groups prog =
  case any (S.member prog) groups of
    True -> groups
    False -> (getConnectedTo prog mappings):groups

getGroups :: [(Int, Set Int)] -> [Set Int]
getGroups mappings = foldl (aggGroups mappings) [] allPrograms
  where
    allPrograms = fst <$> mappings

main :: IO ()
main = do
  input <- lines <$> readData "data\\Day12"
  let connections  =  traverse (Common.parse pRelationship) input
  let p1 = (length . getConnectedTo 0) <$> connections
  let p2 = (length . getGroups) <$> connections
  putStrLn $ "Day 12: (Part 1, Part 2) = " ++ (show (p1, p2)) 