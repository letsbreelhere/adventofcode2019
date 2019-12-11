module Main where

import Data.List (minimumBy)
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)

data Color
  = Black
  | White
  deriving (Eq)

type Pixel = First Color

image :: Int -> Int -> [Int] -> [[Pixel]]
image width height = chunksOf (width * height) . map pixel
  where
    pixel 0 = pure Black
    pixel 1 = pure White
    pixel 2 = mempty

count :: Maybe Color -> [Pixel] -> Int
count c = length . filter (== First c)

groupPixels :: [[Pixel]] -> [Pixel]
groupPixels = foldr (zipWith mappend) (repeat mempty)

draw :: Int -> [Pixel] -> String
draw width =
  unlines . chunksOf width . map (renderChar . fromMaybe Black . getFirst)
  where
    renderChar White = '█'
    renderChar _ = ' '

main = do
  ps <- map (: []) . init <$> getContents
  let im = image 25 6 (map read ps)
      l = minimumBy (comparing (count (Just Black))) im
  -- Part 1
  print $ count (Just White) l * count Nothing l
  -- Part 2
  putStr . draw 25 . groupPixels $ im
