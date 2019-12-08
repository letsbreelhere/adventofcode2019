{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe
import Data.Ord
import Debug.Trace

type Layer a = [[a]]

type Image = [Layer Int]

image :: Int -> Int -> [Int] -> Image
image width height ps =
  let area = width * height
  in map (chunksOf width) (chunksOf area ps)

count :: Int -> Layer Int -> Int
count d = length . filter (== d) . concat

pixel :: [Int] -> Int
pixel = fromMaybe 2 . find (/= 2)

groupPixels :: Image -> Layer [Int]
groupPixels ls =
  let ls' = map (map (map (: []))) ls
      merge = zipWith (zipWith (++))
  in foldl merge (repeat (repeat [])) ls'

draw :: [[Int]] -> String
draw = unlines . map renderRow
  where
    renderRow =
      map $ \case
        0 -> ' '
        1 -> '█'

main = do
  ps <- map (: []) . init <$> getContents
  let im = image 25 6 (map read ps)
      l = minimumBy (comparing (count 0)) im
      rendered = map (map pixel) . groupPixels $ im
  -- Part 1
  print $ count 1 l * count 2 l
  -- Part 2
  putStrLn (draw rendered)
