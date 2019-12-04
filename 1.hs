module DayOne where

import Data.Monoid (Sum(..))

sumBy :: (Traversable t, Num n) => (a -> n) -> t a -> n
sumBy f = getSum . foldMap (Sum . f)

fuelRequired :: Int -> Int
fuelRequired = subtract 2 . (`div` 3)

repeatingFuelRequired :: Int -> Int
repeatingFuelRequired = sum . tail . takeWhile (> 0) . iterate fuelRequired

main :: IO ()
main = do
  values <- map read . lines <$> readFile "1.txt" :: IO [Int]
  print $ sumBy fuelRequired values
  print $ sumBy repeatingFuelRequired values
