module DayOne where

fuelRequired :: Int -> Int
fuelRequired = subtract 2 . (`div` 3)

repeatingFuelRequired :: Int -> Int
repeatingFuelRequired = sum . tail . takeWhile (> 0) . iterate fuelRequired

main :: IO ()
main = do
  values <- map read . lines <$> readFile "1.txt" :: IO [Int]
  print . sum . map fuelRequired $ values
  print . sum . map repeatingFuelRequired $ values
