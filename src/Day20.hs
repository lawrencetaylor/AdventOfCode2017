module Day20 where

import Common
import Text.ParserCombinators.Parsec(Parser)
import Text.Parsec
import Data.List

-- Parsing
pNumber :: Parser Int
pNumber = read <$> many (char '-' <|> digit)

pTriplet :: Char -> Parser (Int, Int, Int)
pTriplet labelChar = do
  char labelChar
  string "=<"
  first <- pNumber
  char ','
  second <- pNumber
  char ','
  third <- pNumber
  char '>'
  return (first, second, third)

pPosition :: Parser Position
pPosition = pTriplet 'p'

pVelocity :: Parser Velocity
pVelocity = pTriplet 'v'

pAcceleration :: Parser Acceleration
pAcceleration = pTriplet 'a'

pParticle :: Parser Particle
pParticle = do
  p <- pPosition
  string ", "
  v <- pVelocity
  string ", "
  a <- pAcceleration
  return $ Particle p v a

-- Solution

type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)
type Acceleration = (Int, Int, Int)
data Particle = 
  Particle {  p :: Position
           , v :: Velocity
           , a :: Acceleration  }
           deriving (Show)
type Path = [(Position, Velocity)]

manhattan :: (Int, Int, Int) -> Int
manhattan (a, b, c) = abs a + abs b + abs c

acceleration :: (Int, Particle) -> (Int, Particle) -> Ordering
acceleration (_, p1) (_, p2) = compare (manhattan $ a p1) (manhattan $ a p2) 

partOne :: [(Int, Particle)] -> Int
partOne particles = fst $ head $ sortBy acceleration particles

path :: Particle -> Path
path (Particle p v a) = iterate (move a) (p, v)
  where 
    move (aX, aY, aZ) ((pX, pY, pZ), (vX, vY, vZ))  = 
      let 
        (vX', vY', vZ') = (vX+aX, vY+aY, vZ+aZ)
      in ((pX + vX', pY + vY', pZ + vZ'), (vX',vY', vZ'))

removeCollisions :: [(Int, Path)] -> [(Int, Path)]
removeCollisions paths = fmap (\(i, (_:t)) -> (i, t)) $ filter (toKeep . fst) paths
  where 
    indexedHeads = fmap (\(i, h:_) -> (i, h)) paths
    areSame (_, (p1, _)) (_, (p2, _)) = p1 == p2
    indicesToKeep = fmap fst $ concat $ filter ((==) 1 . length) $ groupBy areSame indexedHeads
    toKeep i = elem i indicesToKeep

evolution :: [(Int, Path)] -> [[(Int, Path)]]
evolution initial = iterate removeCollisions initial

-- Should really check that all particles are diverging formally...
main :: IO ()
main = do 
  input <- readData "data\\Day20"
  let (Right particles) = traverse (Common.parse pParticle) $ lines input
  putStrLn $ "Number of particles " ++ show (length particles)
  let p1 = partOne $ zip [0..] particles
  let p2 = fmap length $ evolution $ zip [0..] $ fmap path particles
  putStrLn $ "Day 20: (Part 1, Part 2) = " ++ (show (p1, p2 !! 50))