module Day02 where

import Common
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(many1, digit, sepBy, newline, tab, char, (<|>) )

-- Parsing

pInt :: Parser Int
pInt = (read <$> many1 digit :: Parser Int)

pSpaceSep :: Parser Char
pSpaceSep = char ' ' <|> tab

pLine :: Parser [Int]
pLine = sepBy pInt (many1 $ pSpaceSep)

pLines :: Parser [[Int]]
pLines = sepBy pLine newline

sourceData :: IO (Either ParseError [[Int]])
sourceData = fmap (parse pLines) $ readData "data\\Day02"

-- Solution

partOneCheckSum :: [Int] -> Int
partOneCheckSum l = maximum l - minimum l 

partTwoCheckSum :: [Int] -> Int
partTwoCheckSum l = q
  where
    pairs s = (,) <$> s <*> s
    remQuotient (a,b) = (rem a b, quot a b)
    notEqual (a,b) = a /= b
    dp = filter notEqual $ pairs l
    [(_, q)] = filter ((==) 0 . fst) $ fmap remQuotient dp

solve :: ([Int]-> Int) -> [[Int]] -> Int
solve checkSum = sum . fmap checkSum

main :: IO ()
main = do
  contents <- sourceData
  let (Right partOne) = fmap (solve partOneCheckSum) contents
  let (Right partTwo) = fmap (solve partTwoCheckSum) contents
  putStrLn $ "Day 02: (Part 1, Part 2) = " ++ (show (partOne, partTwo))