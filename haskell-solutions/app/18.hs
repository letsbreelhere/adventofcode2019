{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bifunctor
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Linear.V2
import Dir
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M

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
parseRow row (y, l) = (y+1, l')
  where l' = snd $ foldl' parseCell (0, l) row
        parseCell (x, l) c =
          let newCell = case tileFromChar c of
                          Nothing -> M.empty
                          Just t -> M.singleton (V2 x y) t
           in (x+1, M.union newCell l)

tileFromChar :: Char -> Maybe Tile
tileFromChar c = case c of
                   '.' -> Just Floor
                   '@' -> Just Me
                   c | isLetter c ->
                     Just (if isUpper c then Door (toLower c) else Key c)
                   '#' -> Nothing
                   _ -> error ("Unexpected tile " ++ show c)

useKey :: Char -> MazeGraph -> MazeGraph
useKey c = mapVertices (second (openDoor . removeKey))
  where openDoor (Door c') | c' == c = Floor
        openDoor t = t
        removeKey (Key c') | c' == c = Floor
        removeKey t = t

removeDoors :: MazeGraph -> MazeGraph
removeDoors = filterVertices (not . isDoor)
  where isDoor (_, Door _) = True
        isDoor _ = False

withoutKeyChildren :: Tree Dir (Point, Tile) -> Tree Dir (Point, Tile)
withoutKeyChildren (Tree v@(_,t) cs) =
  case t of
    Key _ -> Tree v M.empty
    _ -> Tree v (M.map withoutKeyChildren cs)

filterTree :: (v -> Bool) -> Tree e v -> [v]
filterTree p (Tree v cs) =
  let children = concatMap (filterTree p) (map snd (M.toList cs))
   in if p v
         then v : children
         else children

availableKeys' :: Tree Dir (Point, Tile) -> [(Point, Char)]
availableKeys' (Tree v cs) =
  let rest = concatMap availableKeys' (map snd (M.toList cs))
   in case v of
        (p, Key c) -> (p, c) : rest
        _ -> rest

availableKeys :: Point -> MazeGraph -> [(Point, Char)]
availableKeys p g = availableKeys' (bfs (p, Floor) g)

main :: IO ()
main = do
  input <- readFile "./sample.txt"
  let layout = parseLayout input
      Just (curPos, _) = find ((Me ==) . snd) . M.toList $ layout
      meToFloor t = case t of
                      Me -> Floor
                      _ -> t
      g = mapVertices (second meToFloor) (toDirGraph layout)
      t = bfs (curPos, Floor) $ g
  print (withoutKeyChildren t)
