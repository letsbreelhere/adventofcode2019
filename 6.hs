module Main where

import Safe
import Data.Map (Map)
import Data.Maybe (maybe)
import Data.List
import Data.Tuple
import qualified Data.Map as M

type Tree = Map String String

split' :: Eq a => [a] -> a -> [a] -> ([a], [a])
split' ls c [] = (ls, [])
split' ls c (x:xs)
  | c == x = (ls, xs)
  | otherwise = split' (ls ++ [x]) c xs

split = split' []

edges :: String -> [(String, String)]
edges = map (split ')') . lines

parseTree :: String -> Tree
parseTree = M.fromList . map swap . edges

parentsOf :: Tree -> String -> [String]
parentsOf t s =
  case M.lookup s t of
    Nothing -> []
    Just p -> p : parentsOf t p

heightOf :: Tree -> String -> Int
heightOf t = length . parentsOf t

heightTo :: Tree -> String -> String -> Maybe Int
heightTo t root child
  | root == child = Just 0
  | otherwise = do
    p <- M.lookup child t
    (1 +) <$> heightTo t root p

leastUpperBound :: Tree -> String -> String -> Maybe String
leastUpperBound t l r =
  let lParents = parentsOf t l
      rParents = parentsOf t r
   in headMay (lParents `intersect` rParents)

distanceBetween :: Tree -> String -> String -> Maybe Int
distanceBetween t l r = do
  lub <- leastUpperBound t l r
  lh <- heightTo t lub l
  rh <- heightTo t lub r
  pure (lh + rh)

main :: IO ()
main = do
  t <- parseTree <$> readFile "6.txt"
  -- Part 1
  print . sum . map (heightOf t) $ M.keys t
  -- Part 2
  print . fmap (subtract 2) $ distanceBetween t "YOU" "SAN"
