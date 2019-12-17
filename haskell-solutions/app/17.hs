module Main where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Intcode
import Linear.V2
import Safe
import Dir
import Debug.Trace

data Tile
  = Pipe
  | Space
  | Bot
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
  x <- [minX .. maxX]
  y <- [minY .. maxY]
  let isPipe x' y' = (== Just Pipe) $ M.lookup (V2 x' y') l
      isIntersection =
        and $ do
          d <- [-1, 1]
          pure (isPipe (x + d) y && isPipe x (y + d))
  guard isIntersection
  pure (V2 x y)

parseLayout :: Int -> Map Point Tile -> Point -> [String] -> (Layout, Point)
parseLayout _ m p [] = (m, p)
parseLayout y m p (row:rows) = parseLayout (y + 1) m' p' rows
  where
    (m', p') =
      foldr
        (\(entryPoint, entry, isBot) (curM, curP) ->
           ( M.insert entryPoint entry curM
           , if isBot
               then entryPoint
               else curP))
        (m, p)
        entries
    entries = zipWith (curry parseRow) [0 ..] row
    botChars = "^v<>X" :: String
    parseRow (x, c) =
      let tile =
            case c of
              '.' -> Space
              'X' -> Space
              _ -> Pipe
      in (V2 x y, tile, c `elem` botChars)

isPipe l p = M.lookup p l == Just Pipe

data Move = DeadEnd | Ahead | Turning Turn
  deriving (Show)

nextMove :: Layout -> Point -> Dir -> Move
nextMove l p d
  | isPipe l (moveDir p d) = Ahead
  | otherwise =
    let mturn = find (isPipe l . moveDir p . (`turnDir` d)) [L, R]
     in maybe DeadEnd Turning mturn

route :: Layout -> Point -> [(Turn, Int)]
route l p = tail $ route' p N [] 0 R
  where
    route' pos dir result count lastTurn =
      case nextMove l pos dir of
        DeadEnd -> result
        Ahead -> route' (moveDir pos dir) dir result (count + 1) lastTurn
        Turning t ->
          let dir' = turnDir t dir
              pos' = moveDir pos dir'
              result' = result ++ [(lastTurn, count)]
           in route' pos' dir' result' 0 t

subseqs :: [a] -> [[a]]
subseqs [x] = [[x]]
subseqs as@(x:xs) = tail (inits as) ++ subseqs xs

occurrenceMap :: Ord a => [a] -> Map a Int
occurrenceMap = foldr (\w -> M.insertWith (+) w 1) M.empty

sortedSubseqs :: Ord a => [a] -> [[a]]
sortedSubseqs =
  map fst .
  sortOn (\(l, r) -> length l * r) .
  M.toList .
  M.filter (>1) .
  occurrenceMap . subseqs

main :: IO ()
main = do
  cs <- fromFile "../17.txt"
  os <- view outputs <$> run cs
  let str = map (toEnum . fromIntegral) . toList $ os :: String
      (layout, pos) = parseLayout 0 M.empty (V2 (-1) (-1)) (lines str)
      part1 = sum . map (\(V2 x y) -> x * y) $ intersectionPoints layout
  print part1
  print $ route layout pos
  let awakeCs = cs & code %~ M.insert 0 2
  pure ()
