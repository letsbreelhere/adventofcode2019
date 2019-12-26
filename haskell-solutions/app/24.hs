module Main where

import Data.Bits
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Dir
import Linear.V2

type Grid = Set Point

occupied :: Grid -> Point -> Bool
occupied = flip S.member

inBounds :: Point -> Bool
inBounds (V2 x y) = x `elem` [0 .. 4] && y `elem` [0 .. 4]

neighbors :: Point -> [Point]
neighbors p = filter inBounds . map ((p +) . vectorRep) $ [N, S, E, W]

livingNeighbors :: Grid -> Point -> Int
livingNeighbors g p = length [n | n <- neighbors p, occupied g n]

potentialBirthplaces :: Grid -> Set Point
potentialBirthplaces g = S.fromList [n | p <- S.toList g, n <- neighbors p]

birthplaces :: Grid -> Set Point
birthplaces g =
  S.filter (\p -> not (occupied g p) && livingNeighbors g p `elem` [1, 2]) .
  potentialBirthplaces $
  g

survivors :: Grid -> Set Point
survivors g = S.filter (\p -> livingNeighbors g p == 1) g

step :: Grid -> Grid
step g = S.union (survivors g) (birthplaces g)

parseGrid' :: Int -> Grid -> [String] -> Grid
parseGrid' _ g [] = g
parseGrid' y g (row:rows) = parseGrid' (y + 1) g' rows
  where
    g' = foldr S.insert g entries
    entries = catMaybes $ zipWith hasBug [0 ..] row
    hasBug x c =
      case c of
        '.' -> Nothing
        '#' -> Just (V2 x y)

parseGrid :: [String] -> Grid
parseGrid = parseGrid' 0 S.empty

prettyGrid :: Grid -> String
prettyGrid g =
  unlines $ do
    y <- [0 .. 4]
    pure $ do
      x <- [0 .. 4]
      pure $
        if S.member (V2 x y) g
          then '#'
          else '.'

fromBin :: [Bool] -> Int
fromBin = go 0
  where
    go n [] = n
    go n (x:xs) = go (shiftL n 1 + fromEnum x) xs

biodiversityRating :: Grid -> Int
biodiversityRating g =
  fromBin $ do
    y <- [4,3 .. 0]
    x <- [4,3 .. 0]
    pure $ S.member (V2 x y) g

findRepeat :: (Ord a, Eq a) => [a] -> a
findRepeat = go S.empty
  where
    go _ [] = error "Uh oh"
    go s (x:xs) =
      if S.member x s
        then x
        else go (S.insert x s) xs

main :: IO ()
main = do
  grid <- parseGrid . lines <$> readFile "../24.txt"
  let steps = iterate step grid
      part1 = biodiversityRating (findRepeat steps)
  print part1
