{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad
import Data.List (maximumBy, sortOn)
import Data.Ord
import Data.Set (Set, (\\))
import qualified Data.Set as S

data Point = P
  { x :: Int
  , y :: Int
  } deriving (Eq, Ord, Show)

data Field = Field
  { contents :: Set Point
  , width :: Int
  , height :: Int
  } deriving (Show)

between a a' x =
  let b = min a a'
      b' = max a a'
  in b <= x && x <= b'

-- On a given field, can p1 see p2?
-- Note that this is symmetric, so the ordering of p1 and p2 doesn't matter in later
-- functions.
isVisible :: Field -> Point -> Point -> Bool
isVisible Field {contents, width, height} p1 p2 =
  let is = S.filter (onLine p1 p2) contents
  in not .
     any
       (\p ->
          p /= p1 &&
          p /= p2 && between (x p1) (x p2) (x p) && between (y p1) (y p2) (y p)) $
     is

onLine :: Point -> Point -> Point -> Bool
onLine (P a b) (P c d) (P x y) = y * (c - a) == (d - b) * (x - a) + b * (c - a)

-- On a given field, what can p see?
visiblePoints :: Field -> Point -> Set Point
visiblePoints f@Field {contents} p =
  S.filter (\p' -> p /= p' && isVisible f p p') contents

angle :: Point -> Point -> Float
angle p p' = atan2 (fromIntegral $ y p' - y p) (fromIntegral $ x p' - x p)

flipped :: Point -> Point
flipped P {x, y} = P x (negate y)

-- Because of the clockwise turning + flipping of the y axis, we have to
-- transform provided points to normalize their angles for sorting.
angle' :: Point -> Point -> Float
angle' p1@(P ax ay) p2@(P bx by) =
  let p1' = P (negate ay) ax
      p2' = P (negate by) bx
      a = angle p1' p2'
  in if a >= 0
       then a
       else 2 * pi + a

-- Given a field f and point p, remove all points visible at p from f and return
-- the removed points in angle-sorted order.
blast :: Field -> Point -> ([Point], Field)
blast f@Field {contents} p =
  let vps = visiblePoints f p
      removed = contents \\ vps
      sorted = sortOn (angle' p) (S.toList vps)
  in (sorted, f {contents = removed})

station :: Field -> Point
station field =
  maximumBy (comparing (S.size . visiblePoints field)) . S.toList . contents $
  field

blastingOrder :: Field -> [Point]
blastingOrder field = go field (station field) []
  where
    go f p xs =
      let (ps, f') = blast f p
      in case ps of
           [] -> xs
           _ -> go f' p (xs ++ ps)

mkField :: Int -> Int -> String -> Field
mkField w h points = Field (foldr step S.empty (zip points [0 ..])) w h
  where
    step ('.', _) s = s
    step ('#', i) s = S.insert (P (i `mod` w) (i `div` w)) s

main :: IO ()
main = do
  grid <- lines <$> getContents
  let w = length (head grid)
      h = length grid
      field = mkField w h (concat grid)
      part1 =
        maximum . map (S.size . visiblePoints field) . S.toList . contents $
        field
  print part1
  let P {x, y} = blastingOrder field !! 199
      part2 = x * 100 + y
  print part2
