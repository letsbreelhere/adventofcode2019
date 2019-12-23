{-# LANGUAGE ViewPatterns, TupleSections #-}
module Graph where

import Data.Bifunctor
import Debug.Trace
import Data.Sequence (Seq, viewl, ViewL(..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Control.Lens hiding ((:<))
import Dir

data Graph e v = Graph { getAdj :: Map v (Map e v) }
  deriving (Show, Eq)

-- Can' be a functor because it induces a constraint on the type argument :(
mapVertices :: (Ord v, Ord v') => (v -> v') -> Graph e v -> Graph e v'
mapVertices f = Graph . M.mapKeys f . M.map (M.map f) . getAdj

neighbors :: Ord v => v -> Graph e v -> Map e v
neighbors v = fromMaybe M.empty . M.lookup v . getAdj

addEdge :: (Ord v, Ord e) => v -> e -> v -> Graph e v -> Graph e v
addEdge v e v' (Graph g) = Graph $ M.insertWith M.union v (M.singleton e v') g

emptyGraph = Graph M.empty

data Tree e v =
  Tree v
       (Map e (Tree e v))

instance Functor (Tree e) where
  fmap f (Tree v m) = Tree (f v) (M.map (fmap f) m)

-- I guess there isn't a name for "commutatively foldable". Oh well, close
-- enough.
instance Foldable (Tree e) where
  foldMap f (Tree v cs) =
    let folded = foldMap (foldMap f) cs
     in f v <> folded

withHeights :: Tree e v -> Tree e (v, Int)
withHeights (Tree v cs) = Tree (v, 0) (M.map (fmap (second (+1)) . withHeights) cs)

showTree :: (Show v, Show e) => Int -> Tree e v -> [String]
showTree indent (Tree v cs) =
  let children = M.toList cs
      tabs = concat $ replicate indent "  "
   in (tabs ++ show v) : concatMap (\(e, t') -> (tabs ++ show e ++ ":") : showTree (indent+1) t') children

instance (Show v, Show e) => Show (Tree e v) where
  show = unlines . showTree 0

toTree' :: (Ord e, Ord v) => Set v -> v -> Graph e v -> Maybe (Tree e v)
toTree' s v g
  | S.member v s = Nothing
  | otherwise = Just $ Tree v (M.fromList . mapMaybe (\(e,v') -> (e,) <$> toTree' (S.insert v s) v' g) . M.toList $ neighbors v g)

toTree :: (Ord e, Ord v) => v -> Graph e v -> Tree e v
toTree v g = fromJust $ toTree' S.empty v g

toDirGraph :: Ord v => Map Point v -> Graph Dir (Point, v)
toDirGraph m = Graph . M.fromList . map (\(p, v) -> ((p, v), neighbors p)) . M.toList $ m
  where neighbors p = M.fromList $ mapMaybe (neighborAt p) [N, E, W, S]
        neighborAt p dir = do
          let p' = (p + vectorRep dir)
          v' <- M.lookup p' m
          pure (dir, (p', v'))

addEdgeUnlessVisited :: (Ord v, Ord e) => Set v -> v -> e -> v -> Graph e v -> Graph e v
addEdgeUnlessVisited visited v e v' (Graph g)
  | S.member v' visited = Graph g
  | otherwise = Graph $ M.insertWith M.union v (M.singleton e v') g

deleteVertex :: (Ord v, Ord e) => v -> Graph e v -> Graph e v
deleteVertex v = filterVertices (/= v)

filterVertices :: (Ord v, Ord e) => (v -> Bool) -> Graph e v -> Graph e v
filterVertices p (Graph g) =
  let g' = M.filterWithKey (\v _ -> p v) g
      removeChildren = M.filter p
   in Graph (M.map removeChildren g')

dfs' :: (Ord v, Ord e) => [(Maybe (v, e), v)] -> Set v -> Graph e v -> Graph e v -> Graph e v
dfs' [] _ out _ = out
dfs' ((parentData, v) : toVisit) visited out g
  | S.member v visited = dfs' toVisit visited out g
  | otherwise =
    let curNeighbors = M.toList $ neighbors v g
        visited' = S.insert v visited
        out' = case parentData of
                 Just (parent, e) -> addEdgeUnlessVisited visited parent e v out
                 Nothing -> out
        toVisit' = (map (\(e, v') -> (Just (v, e), v')) curNeighbors) ++ toVisit
    in dfs' toVisit' visited' out' g

dfs :: (Ord v, Ord e) => v -> Graph e v -> Tree e v
dfs v = toTree v . dfs' [(Nothing, v)] S.empty emptyGraph

bfs' :: (Ord v, Ord e) => Seq (Maybe (v, e), v) -> Set v -> Graph e v -> Graph e v -> Graph e v
bfs' (viewl -> EmptyL) _ out _ = out
bfs' (viewl -> (parentData, v) :< toVisit) visited out g
  | S.member v visited = bfs' toVisit visited out g
  | otherwise =
    let curNeighbors = M.toList $ neighbors v g
        visited' = S.insert v visited
        out' = case parentData of
                 Just (parent, e) -> addEdgeUnlessVisited visited parent e v out
                 Nothing -> out
        toVisit' = toVisit <> Seq.fromList (map (\(e, v') -> (Just (v, e), v')) curNeighbors)
    in bfs' toVisit' visited' out' g

bfs :: (Ord v, Ord e) => v -> Graph e v -> Tree e v
bfs v = toTree v . bfs' (Seq.singleton (Nothing, v)) S.empty emptyGraph

height :: Tree e v -> Int
height (Tree _ cs) =
  let heights = map (\(_, t') -> 1 + height t') (M.toList cs)
   in maximum (0 : heights)
