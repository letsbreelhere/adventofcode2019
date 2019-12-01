module DayOne where

import Data.Monoid (Sum(..))

fuelRequired :: Int -> Int
fuelRequired x = x `div` 3 - 2

part1 :: [Int] -> Int
part1 values = getSum . foldMap (Sum . fuelRequired) $ values

repeatingFuelRequired :: Int -> Int
repeatingFuelRequired x =
  let init = fuelRequired x
   in if init <= 0
         then 0
         else init + repeatingFuelRequired init

part2 :: [Int] -> Int
part2 values = getSum . foldMap (Sum . repeatingFuelRequired) $ values

main = do
  values <- map read . lines <$> readFile "1.txt" :: IO [Int]
  print $ part1 values
  print $ part2 values
