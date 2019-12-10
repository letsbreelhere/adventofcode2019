{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad
import Data.List ((\\), maximumBy, sortBy)
import Data.Ord
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as S

data Point = P
  { x :: Int
  , y :: Int
  } deriving (Eq, Ord, Show)

diff :: Point -> Point -> Point
diff (P a b) (P c d) = P (a - c) (b - d)

cross :: Point -> Point -> Int
cross (P ax ay) (P bx by) = ax * by - ay * bx

data Field = Field
  { contents :: [Point]
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
  let is = filter (collinear p1 p2) contents
  in not .
     any
       (\p ->
          p /= p1 &&
          p /= p2 && between (x p1) (x p2) (x p) && between (y p1) (y p2) (y p)) $
     is

collinear :: Point -> Point -> Point -> Bool
collinear a b c = cross (diff a b) (diff a c) == 0

-- On a given field, what can p see?
visiblePoints :: Field -> Point -> [Point]
visiblePoints f@Field {contents} p =
  filter (\p' -> p /= p' && isVisible f p p') contents

data Dir
  = L
  | R
  deriving (Eq, Ord)

data Slope
  = Vertical
  | Slant Dir
          (Ratio Int)
  deriving (Eq, Ord)

slope P {x, y}
  | x == 0 = Vertical
  | otherwise =
    Slant
      (if x < 0
         then L
         else R)
      (y % x)

visiblePoints' :: Field -> Point -> Set Slope
visiblePoints' f p =
  foldr (\p' -> S.insert (slope (diff p' p))) S.empty (contents f)

compareAngle :: Point -> Point -> Point -> Ordering
compareAngle c p p'
  | da /= db = compare da db
  | ax == 0 && bx == 0 = signum ay `compare` signum by
  | otherwise = compare (signum (cross b a)) 0
  where
    a@(P ax ay) = diff p c
    b@(P bx by) = diff p' c
    da = ax < 0
    db = bx < 0

-- Given a field f and point p, remove all points visible at p from f and return
-- the removed points in angle-sorted order.
blast :: Field -> Point -> ([Point], Field)
blast f@Field {contents} p =
  let vps = visiblePoints f p
      removed = contents \\ vps
      sorted = sortBy (compareAngle p) vps
  in (sorted, f {contents = removed})

station :: Field -> Point
station field =
  maximumBy (comparing (S.size . visiblePoints' field)) . contents $ field

blastingOrder :: Field -> Point -> [Point]
blastingOrder field s = go field s []
  where
    go f p xs =
      let (ps, f') = blast f p
      in case ps of
           [] -> xs
           _ -> go f' p (xs ++ ps)

mkField :: Int -> Int -> String -> Field
mkField w h points = Field (foldr step [] (zip points [0 ..])) w h
  where
    step ('.', _) s = s
    step ('#', i) s = P (i `mod` w) (i `div` w) : s

main :: IO ()
main = do
  grid <- lines <$> getContents
  let w = length (head grid)
      h = length grid
      field = mkField w h (concat grid)
      s = station field
      part1 = length . visiblePoints field $ s
  print part1
  let P {x, y} = blastingOrder field s !! 199
      part2 = x * 100 + y
  print part2
