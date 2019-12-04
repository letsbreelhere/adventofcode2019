module DayFour where

import Data.List (group)

monotonic :: Ord a => [a] -> Bool
monotonic [] = True
monotonic xs = and $ zipWith (<=) xs (tail xs)

adjacentEqual :: Eq a => [a] -> Bool
adjacentEqual [] = False
adjacentEqual xs = or $ zipWith (==) xs (tail xs)

isValidPassword :: Int -> Bool
isValidPassword x =
  let ds = show x
  in monotonic ds && adjacentEqual ds

containsDouble :: Eq a => [a] -> Bool
containsDouble = any ((== 2) . length) . group

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
