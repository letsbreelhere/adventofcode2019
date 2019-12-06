{-# LANGUAGE NamedFieldPuns #-}

module DaySix where

import Data.List
import Debug.Trace

data Tree a
  = Top [Tree a]
  | Tree { tip :: a
         , children :: [Tree a] }

instance Show a => Show (Tree a) where
  show (Tree x []) = show x
  show (Tree x xs) = show x ++ " -> (" ++ intercalate ", " (map show xs) ++ ")"
  show (Top xs) = "! -> (" ++ intercalate ", " (map show xs) ++ ")"

contains :: Eq a => a -> Tree a -> Bool
contains x (Top ts) = any (contains x) ts
contains x Tree{tip, children} = x == tip || any (contains x) children

addEdge :: Eq a => a -> a -> Tree a -> Tree a
addEdge p c t@Tree {tip, children}
  | p == tip =
    let child = Tree c []
    in Tree tip (children ++ [child])
  | c == tip = Tree p [t]
  | any (contains p) children || any (contains c) children = Tree tip (map (addEdge p c) children)
  | otherwise = t
addEdge p c (Top ts)
  | any (contains p) ts || any (contains c) ts = Top (map (addEdge p c) ts)
  | otherwise = Top (ts ++ [Tree p [Tree c []]])

split' :: Eq a => [a] -> a -> [a] -> ([a], [a])
split' ls c [] = (ls, [])
split' ls c (x:xs)
  | c == x = (ls, xs)
  | otherwise = split' (ls ++ [x]) c xs

split = split' []

edges :: String -> [(String, String)]
edges = map (split ')') . lines

parseTree :: String -> Tree String
parseTree input =
  let pairs = edges input
      top = fst (head pairs)
  in foldl' (\t (p, c) -> addEdge p c t) (Top []) pairs

totalHeights :: Show a => Int -> Tree a -> Int
totalHeights h (Top ts) = sum . map (totalHeights h) $ ts
totalHeights h Tree{tip, children} = traceShow (tip, h) f
  where f = let total = sum . map (totalHeights (h+1)) $ children
             in h + total

main :: IO ()
main = do
  input <- parseTree <$> readFile "6sample.txt"
  -- Part 1
  print input
  --print (totalHeights 0 input)
  -- Part 2
