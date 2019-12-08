module Main where

import Data.List (minimumBy)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)

type Layer a = [a]

type Image = [Layer Pixel]

newtype Pixel =
  Pixel Int
  deriving (Eq, Num)

instance Semigroup Pixel where
  2 <> p = p
  p <> _ = p

image :: Int -> Int -> [Int] -> Image
image width height = chunksOf (width * height) . map Pixel

count :: Int -> Layer Pixel -> Int
count d = length . filter (== Pixel d)

groupPixels :: Image -> Layer Pixel
groupPixels = foldr (zipWith (<>)) (repeat (Pixel 2))

draw :: Int -> Layer Pixel -> String
draw width = unlines . chunksOf width . map renderChar
  where
    renderChar =
      \case
        1 -> '█'
        _ -> ' '

main = do
  ps <- map (: []) . init <$> getContents
  let im = image 25 6 (map read ps)
      l = minimumBy (comparing (count 0)) im
      rendered = groupPixels im
  -- Part 1
  print $ count 1 l * count 2 l
  -- Part 2
  putStrLn (draw 25 rendered)
