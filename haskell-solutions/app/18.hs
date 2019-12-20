{-# LANGUAGE TupleSections #-}

module Main where

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
                     Just (if isUpper c then Door c else Key c)
                   '#' -> Nothing
                   _ -> error ("Unexpected tile " ++ show c)

main :: IO ()
main = do
  input <- readFile "sample.txt"
  let layout = parseLayout input
      Just curPos = fmap fst . find ((Me ==) . snd) . M.toList $ layout
      g = toDirGraph layout
      t = bfs (curPos, Me) g
  print t
