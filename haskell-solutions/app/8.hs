module Main where

import Data.List (minimumBy)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)

newtype Pixel =
  Pixel Int
  deriving (Eq, Num)

instance Semigroup Pixel where
  2 <> p = p
  p <> _ = p

image :: Int -> Int -> [Int] -> [[Pixel]]
image width height = chunksOf (width * height) . map Pixel

count :: Int -> [Pixel] -> Int
count d = length . filter (== Pixel d)

groupPixels :: [[Pixel]] -> [Pixel]
groupPixels = foldr (zipWith (<>)) (repeat 2)

draw :: Int -> [Pixel] -> String
draw width = unlines . chunksOf width . map renderChar
  where
    renderChar 1 = '█'
    renderChar _ = ' '

main = do
  ps <- map (: []) . init <$> getContents
  let im = image 25 6 (map read ps)
      l = minimumBy (comparing (count 0)) im
  -- Part 1
  print $ count 1 l * count 2 l
  -- Part 2
  putStr . draw 25 . groupPixels $ im
