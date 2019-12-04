{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module DayThree where

import Data.Foldable (toList)
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import Control.Applicative (liftA2)
import Control.Monad (guard, foldM)
import Data.Functor (($>))
import Data.List (foldl', minimum)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Text (Text)
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Point = Point { x :: Int, y :: Int }
  deriving (Show, Eq)

origin :: Point
origin = Point 0 0

distance :: Point -> Point -> Int
distance p p' = abs (x p - x p') + abs (y p - y p')

data Line = Line { start :: Point, finish :: Point }
  deriving (Show, Eq)

type Wire = Seq Line

-- This uses Manhattan distance, so it only works if we assume all lines are
-- vertical or horizontal.
pointIsOnLine :: Point -> Line -> Bool
pointIsOnLine p Line{ start, finish } =
  distance start p + distance p finish == distance start finish

isVertical :: Line -> Bool
isVertical Line{start, finish} = y start /= y finish

between a a' x = let b = min a a'
                     b' = max a a'
                  in b <= x && x <= b'

intersect :: Line -> Line -> Maybe Point
intersect l1 l2
  | isVertical l1 =
    let l1x = x (start l1)
        l2y = y (start l2)
        p = Point l1x l2y
     in if pointIsOnLine p l1 && pointIsOnLine p l2
           then Just p
           else Nothing
  | isVertical l2 = l2 `intersect` l1
  | otherwise = Nothing

wireIntersections :: Wire -> Wire -> [Point]
wireIntersections ls ls' =
  filter (/= origin) . catMaybes . toList $ liftA2 intersect ls ls'

lineDistanceToPoint :: Line -> Point -> Maybe Int
lineDistanceToPoint l p = do
  guard (pointIsOnLine p l)
  pure $ distance (start l) p

wireDistanceToPoint :: Wire -> Point -> Maybe Int
wireDistanceToPoint Seq.Empty _ = Nothing
wireDistanceToPoint (l :<| ls) p =
  case lineDistanceToPoint l p of
    Nothing -> fmap (lineLength l +) (wireDistanceToPoint ls p)
    Just d -> Just d
  where lineLength Line{start, finish} = distance start finish

minimizedSumDistance :: Wire -> Wire -> Int
minimizedSumDistance w1 w2 =
  let intersections = wireIntersections w1 w2
      distances =
        flip mapMaybe intersections $ \p -> do
          d1 <- wireDistanceToPoint w1 p
          d2 <- wireDistanceToPoint w2 p
          pure (d1 + d2)
   in minimum distances

------------------------------------PARSING------------------------------------

parseWireStep :: (Wire, Point) -> Text -> Maybe (Wire, Point)
parseWireStep (ls, p@Point{x,y}) t = do
  (c, rest) <- T.uncons t
  d <- readMaybe (T.unpack rest) :: Maybe Int
  let p' = case c of
         'L' -> p { x = x - d }
         'R' -> p { x = x + d }
         'D' -> p { y = y - d }
         'U' -> p { y = y + d }
      l = Line p p'
  pure (ls |> l, p')

parseWire :: Text -> Maybe Wire
parseWire = fmap fst . foldM parseWireStep (Seq.empty, origin) . T.splitOn ","

parseWirePair :: Text -> Maybe (Wire, Wire)
parseWirePair input = let (a:b:_) = T.splitOn "\n" input
                       in liftA2 (,) (parseWire a) (parseWire b)

--------------------------------------MAIN--------------------------------------

main :: IO ()
main = do
  Just (w1, w2) <- parseWirePair <$> T.readFile "3.txt"
  -- Part one
  print . minimum . filter (>0) . map (distance origin) $ wireIntersections w1 w2
  -- Part two
  print $ minimizedSumDistance w1 w2
