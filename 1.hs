module DayOne where

import Data.Monoid (Sum(..))

sumBy :: (Traversable t, Num n) => (a -> n) -> t a -> n
sumBy f = getSum . foldMap (Sum . f)

fuelRequired :: Int -> Int
fuelRequired = subtract 2 . (`div` 3)

part1 :: [Int] -> Int
part1 = sumBy fuelRequired

repeatingFuelRequired :: Int -> Int
repeatingFuelRequired = sum . tail . takeWhile (>0) . iterate fuelRequired

part2 :: [Int] -> Int
part2 = sumBy repeatingFuelRequired

main :: IO ()
main = do
  values <- map read . lines <$> readFile "1.txt" :: IO [Int]
  print $ part1 values
  print $ part2 values
