module Main where

import Control.Monad
import Data.Bits
import Data.Maybe
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Dir
import Control.Lens
import Linear.V2

data PointWithDepth = PWD
  { _point :: Point
  , _depth :: Int
  }
  deriving (Eq, Ord)

instance Show PointWithDepth where
  show (PWD (V2 x y) d) = "<" ++ intercalate ", " (map show [x, y, d]) ++ ">"

makeLenses ''PointWithDepth

type Grid = Set PointWithDepth

occupied :: Grid -> PointWithDepth -> Bool
occupied = flip S.member

standardNeighbor :: PointWithDepth -> Dir -> PointWithDepth
standardNeighbor (PWD p d) dir = PWD (p + vectorRep dir) d

invertPoint :: PointWithDepth -> PointWithDepth
invertPoint (PWD (V2 x y) d) = PWD (V2 y x) d

westNeighbor :: PointWithDepth -> [PointWithDepth]
westNeighbor pwd@(PWD point depth) =
  case point of
    V2 0 _ -> [PWD (V2 1 2) (depth - 1)]
    V2 3 2 -> [PWD p' (depth+1) | y <- [0..4], let p' = V2 4 y]
    _ -> [standardNeighbor pwd W]

eastNeighbor :: PointWithDepth -> [PointWithDepth]
eastNeighbor pwd@(PWD point depth) =
  case point of
    V2 4 _ -> [PWD (V2 3 2) (depth - 1)]
    V2 1 2 -> [PWD p' (depth+1) | y <- [0..4], let p' = V2 0 y]
    _ -> [standardNeighbor pwd E]

northNeighbor :: PointWithDepth -> [PointWithDepth]
northNeighbor = map invertPoint . eastNeighbor . invertPoint

southNeighbor :: PointWithDepth -> [PointWithDepth]
southNeighbor = map invertPoint . westNeighbor . invertPoint

neighbors :: PointWithDepth -> [PointWithDepth]
neighbors p = concatMap ($ p) [northNeighbor, southNeighbor, eastNeighbor, westNeighbor]

livingNeighbors :: Grid -> PointWithDepth -> Int
livingNeighbors g p = length [n | n <- neighbors p, occupied g n]

potentialBirthplaces :: Grid -> Set PointWithDepth
potentialBirthplaces g = S.fromList [n | p <- S.toList g, n <- neighbors p]

birthplaces :: Grid -> Set PointWithDepth
birthplaces g =
  S.filter (\p -> not (occupied g p) && livingNeighbors g p `elem` [1, 2]) .
  potentialBirthplaces $
  g

survivors :: Grid -> Set PointWithDepth
survivors g = S.filter (\p -> livingNeighbors g p == 1) g

step :: Grid -> Grid
step g = S.union (survivors g) (birthplaces g)

parseGrid' :: Int -> Grid -> [String] -> Grid
parseGrid' _ g [] = g
parseGrid' y g (row:rows) = parseGrid' (y + 1) g' rows
  where
    g' = foldr S.insert g entries
    entries = catMaybes $ zipWith hasBug [0 ..] row
    hasBug x c =
      case c of
        '.' -> Nothing
        '#' -> Just $ PWD (V2 x y) 0

parseGrid :: [String] -> Grid
parseGrid = parseGrid' 0 S.empty

prettyGrid :: Int -> Grid -> String
prettyGrid d g =
  unlines $ do
    y <- [0 .. 4]
    pure $ do
      x <- [0 .. 4]
      let p = PWD (V2 x y) d
      pure $
        if S.member p g
          then '#'
          else '.'

main :: IO ()
main = do
  grid <- parseGrid . lines <$> readFile "../24.txt"
  let steps = iterate step grid
  print (S.size (steps !! 200))
