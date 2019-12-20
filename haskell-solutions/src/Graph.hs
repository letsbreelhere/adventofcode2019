{-# LANGUAGE ViewPatterns #-}
module Graph where

import Data.Sequence (Seq, viewl, ViewL(..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Control.Lens hiding ((:<))
import Dir

data Graph v e = Graph { getAdj :: Map v (Map e v) }

neighbors :: Ord v => v -> Graph v e -> Map e v
neighbors v = fromMaybe M.empty . M.lookup v . getAdj

addEdge :: (Ord v, Ord e) => v -> e -> v -> Graph v e -> Graph v e
addEdge v e v' (Graph g) = Graph $ M.insertWith M.union v (M.singleton e v') g

emptyGraph = Graph M.empty

data Tree v e =
  Tree v
       (Map e (Tree v e))
  deriving (Show)

toTree :: Ord v => v -> Graph v e -> Tree v e
toTree v g = Tree v (M.map (\v' -> toTree v' g) $ neighbors v g)

toDirGraph :: Ord v => Map Point v -> Graph v Dir
toDirGraph m = Graph . M.fromList . map (\(p, v) -> (v, neighbors p)) . M.toList $ m
  where neighbors p = M.fromList $ mapMaybe (neighborAt p) [N, E, W, S]
        neighborAt p dir = do
          let p' = (p + vectorRep dir)
          v' <- M.lookup p' m
          pure (dir, v')

dfs' :: (Ord v, Ord e) => [v] -> Set v -> Graph v e -> Graph v e -> Graph v e
dfs' [] _ out _ = out
dfs' (v:toVisit) visited out g
  | S.member v visited = dfs' toVisit visited out g
  | otherwise =
    let curNeighbors = M.toList $ neighbors v g
        visited' = S.insert v visited
        out' = foldr (\(e, v') -> addEdge v e v') out curNeighbors
        toVisit' = map snd curNeighbors ++ toVisit
    in dfs' toVisit' visited' out' g

dfs :: (Ord v, Ord e) => v -> Graph v e -> Tree v e
dfs v = toTree v . dfs' [v] S.empty emptyGraph

bfs' :: (Ord v, Ord e) => Seq v -> Set v -> Graph v e -> Graph v e -> Graph v e
bfs' (viewl -> EmptyL) _ out _ = out
bfs' (viewl -> v :< toVisit) visited out g
  | S.member v visited = bfs' toVisit visited out g
  | otherwise =
    let curNeighbors = M.toList $ neighbors v g
        visited' = S.insert v visited
        out' = foldr (\(e, v') -> addEdge v e v') out curNeighbors
        toVisit' = Seq.fromList (map snd curNeighbors) <> toVisit
    in bfs' toVisit' visited' out' g

bfs :: (Ord v, Ord e) => v -> Graph v e -> Tree v e
bfs v = toTree v . dfs' [v] S.empty emptyGraph
