module DayFour where

import Data.Foldable (Foldable, foldl')
import Data.Maybe (isJust)

monotonic' :: Ord a => a -> [a] -> Bool
monotonic' y (x:xs) = y <= x && monotonic' x xs
monotonic' _ [] = True

monotonic :: Ord a => [a] -> Bool
monotonic [] = True
monotonic (x:xs) = monotonic' x xs

adjacentEqual :: Eq a => [a] -> Bool
adjacentEqual (x:x':xs) = x == x' || adjacentEqual (x' : xs)
adjacentEqual _ = False

isValidPassword :: Int -> Bool
isValidPassword x =
  let ds = show x
  in monotonic ds && adjacentEqual ds

containsDouble :: Eq a => [a] -> Bool
containsDouble ys@(x:x':xs) =
  let (ls, rs) = span (== x) ys
  in length ls == 2 || containsDouble rs
containsDouble _ = False

isValidPassword' :: Int -> Bool
isValidPassword' x =
  let ds = show x
  in monotonic ds && containsDouble ds

input :: [Int]
input = [145852 .. 616942]

main :: IO ()
main = do
  print . length . filter isValidPassword $ input
  print . length . filter isValidPassword' $ input
