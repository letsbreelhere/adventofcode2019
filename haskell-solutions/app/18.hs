{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bifunctor
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Dir
import Linear.V2

import Graph

data Tile
  = Floor
  | Key Char
  | Door Char
  | Me
  deriving (Show, Eq, Ord)

type Layout = Map Point Tile

type MazeGraph = Graph Dir (Point, Tile)

parseLayout :: String -> Layout
parseLayout = snd . foldr parseRow (0, M.empty) . lines

parseRow :: String -> (Int, Layout) -> (Int, Layout)
parseRow row (y, l) = (y + 1, l')
  where
    l' = snd $ foldl' parseCell (0, l) row
    parseCell (x, l) c =
      let newCell =
            case tileFromChar c of
              Nothing -> M.empty
              Just t -> M.singleton (V2 x y) t
       in (x + 1, M.union newCell l)

tileFromChar :: Char -> Maybe Tile
tileFromChar c =
  case c of
    '.' -> Just Floor
    '@' -> Just Me
    c
      | isLetter c ->
        Just
          (if isUpper c
             then Door (toLower c)
             else Key c)
    '#' -> Nothing
    _ -> error ("Unexpected tile " ++ show c)

useKey :: Char -> MazeGraph -> MazeGraph
useKey c = mapVertices (second (openDoor . removeKey))
  where
    openDoor (Door c')
      | c' == c = Floor
    openDoor t = t
    removeKey (Key c')
      | c' == c = Floor
    removeKey t = t

removeDoors :: MazeGraph -> MazeGraph
removeDoors = filterVertices notDoor
  where
    notDoor (_, Door _) = False
    notDoor _ = True

withoutKeyChildren :: Tree Dir (Point, Tile) -> Tree Dir (Point, Tile)
withoutKeyChildren (Tree v@(_, t) cs) =
  case t of
    Key _ -> Tree v M.empty
    _ -> Tree v (M.map withoutKeyChildren cs)

availableKeys' :: Tree a ((Point, Tile), Int) -> [(Point, Int, Char)]
availableKeys' (Tree v cs) =
  let rest = concatMap availableKeys' (map snd (M.toList cs))
   in case v of
        ((p, Key c), h) -> (p, h, c) : rest
        _ -> rest

availableKeys :: Point -> MazeGraph -> [(Point, Int, Char)]
availableKeys p = availableKeys' . withHeights . withoutKeyChildren . bfs (p, Floor) . removeDoors

allKeysCollected :: MazeGraph -> Bool
allKeysCollected = (emptyGraph ==) . filterVertices (isKey . snd)
  where
    isKey (Key _) = True
    isKey _ = False

-- From an initial condition, generate all possible collection routes
-- (edge-labelled by key choice).
keyCollectionRoutes' :: Point -> MazeGraph -> Tree (Char, Int) MazeGraph
keyCollectionRoutes' p g =
  let choices = availableKeys p g
      routeWithChoice (p', h, c) = ((c, h), keyCollectionRoutes' p' (useKey c g))
   in  Tree g (M.fromList $ map routeWithChoice choices)

keyCollectionRoutes :: Point -> MazeGraph -> Tree (Char, Int) ()
keyCollectionRoutes p g = fmap (const ()) (keyCollectionRoutes' p g)

scannedSum :: (e -> Int) -> Tree e v -> Tree e (v, Int)
scannedSum f (Tree v cs) = Tree (v, 0) (M.mapWithKey (\e t' -> fmap (second (+ (f e))) . scannedSum f $ t') cs)

leaves :: Tree e v -> [v]
leaves (Tree v cs) | M.null cs = [v]
leaves (Tree v cs) = concatMap (leaves . snd) (M.toList cs)

main :: IO ()
main = do
  input <- getContents
  let layout = parseLayout input
      Just (curPos, _) = find ((Me ==) . snd) . M.toList $ layout
      meToFloor t =
        case t of
          Me -> Floor
          _ -> t
      g = mapVertices (second meToFloor) (toDirGraph layout)
  print g
  --print . scannedSum snd . keyCollectionRoutes curPos $ g
