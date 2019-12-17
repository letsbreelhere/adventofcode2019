module Main where

import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Intcode
import Linear.V2
import Safe

type Point = V2 Int

data Tile
  = Pipe
  | Floor
  deriving (Show, Eq)

type Layout = Map Point Tile

bounds :: Layout -> (Point, Point)
bounds l =
  let ps = M.keys l
      maxX = maximum $ map (view _x) ps
      maxY = maximum $ map (view _y) ps
      minX = minimum $ map (view _x) ps
      minY = minimum $ map (view _y) ps
  in (V2 minX minY, V2 maxX maxY)

intersectionPoints :: Layout -> [Point]
intersectionPoints l = do
  let (V2 minX minY, V2 maxX maxY) = bounds l
  x <- [minX+1..maxX-1]
  y <- [minY+1..maxY-1]
  let isPipe x' y' = (== Just Pipe) $ M.lookup (V2 x' y') l
      isIntersection = and $ do
        d <- [-1,1]
        pure (isPipe (x+d) y && isPipe x (y+d))
  guard isIntersection
  pure (V2 x y)

parseLayout :: Int -> Map Point Tile -> [String] -> Layout
parseLayout _ m [] = m
parseLayout y m (row:rows) =
  parseLayout (y + 1) (foldr (uncurry M.insert) m entries) rows
  where
    entries = zipWith (curry parseRow) [0 ..] row
    parseRow (x, c) =
      let tile =
            if c == '.'
              then Floor
              else Pipe
      in (V2 x y, tile)

main :: IO ()
main = do
  cs <- fromFile "../17.txt"
  os <- view outputs <$> run cs
  let str = map (toEnum . fromIntegral) . toList $ os :: String
      layout = parseLayout 0 M.empty (lines str)
      part1 = sum . map (\(V2 x y) -> x * y) $ intersectionPoints layout
  print part1

  let awakeCs = cs & code %~ M.insert 0 2
  pure ()
