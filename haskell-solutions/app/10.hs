{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Ord
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as S
import Data.List

data Point = P
  { x :: Int
  , y :: Int
  } deriving (Eq, Ord, Show)

data Field = Field
  { contents :: Set Point
  , width :: Int
  , height :: Int
  } deriving (Show)

-- Given two points, return all lattice points on the line they share within the
-- boundaries.
latticePoints :: Point -> Point -> Int -> Int -> Set Point
latticePoints (P a b) (P c d) maxX maxY =
  S.fromList $ do
    x <- [0 .. maxX]
    y <- [0 .. maxY]
    guard $ y * (c - a) == (d - b) * (x - a) + b * (c - a)
    pure $ P x y

between a a' x =
  let b = min a a'
      b' = max a a'
  in b <= x && x <= b'

-- On a given field, can p1 see p2?
-- Note that this is symmetric, so the ordering of p1 and p2 doesn't matter in later
-- functions.
isVisible :: Field -> Point -> Point -> Bool
isVisible Field {contents, width, height} p1 p2 =
  let lps = latticePoints p1 p2 width height
      is = S.toList $ S.intersection lps contents
  in not .
     any
       (\p ->
          p /= p1 &&
          p /= p2 && between (x p1) (x p2) (x p) && between (y p1) (y p2) (y p)) $
     is

-- On a given field, what can p see?
visiblePoints :: Field -> Point -> Set Point
visiblePoints f@Field {contents} p =
  S.filter (\p' -> p /= p' && isVisible f p p') contents

mkField :: Int -> Int -> String -> Field
mkField w h points = Field (foldr step S.empty (zip points [0 ..])) w h
  where
    step ('.', _) s = s
    step ('#', i) s = S.insert (P (i `mod` w) (i `div` w)) s

angle :: Point -> Point -> Float
angle p p' = if x p == x p'
                then pi/2
                else atan $ fromIntegral (y p' - y p) / fromIntegral (x p' - x p)

main :: IO ()
main = do
  grid <- lines <$> getContents
  let w = length (head grid)
      h = length grid
      field = mkField w h (concat grid)
      part1 = maximum . map (S.size . visiblePoints field) . S.toList . contents $ field
  print part1
  let station = maximumBy (comparing (S.size . visiblePoints field)) . S.toList . contents $ field
  pure ()
