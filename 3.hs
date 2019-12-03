{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module DayThree where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Functor (($>))
import Data.List (foldl', minimum)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Point = Point { x :: Int, y :: Int }
  deriving (Show, Eq)

origin :: Point
origin = Point 0 0

data Line = Line { start :: Point, finish :: Point }
  deriving (Show, Eq)

distance :: Point -> Point -> Int
distance p p' = abs (x p - x p') + abs (y p - y p')

-- This uses Manhattan distance, so it only works if we assume all lines are
-- vertical or horizontal.
pointIsOnLine :: Point -> Line -> Bool
pointIsOnLine p Line{ start, finish } =
  distance start p + distance p finish == distance start finish

isVertical :: Line -> Bool
isVertical l = y (start l) /= y (finish l)

isHorizontal :: Line -> Bool
isHorizontal = not . isVertical

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

wireIntersections :: [Line] -> [Line] -> [Point]
wireIntersections ls ls' =
  filter (/= origin) . catMaybes $ liftA2 intersect ls ls'

lineDistanceToPoint :: Line -> Point -> Maybe Int
lineDistanceToPoint l p = do
  guard (pointIsOnLine p l)
  pure $ distance (start l) p

wireDistanceToPoint :: [Line] -> Point -> Maybe Int
wireDistanceToPoint [] _ = Nothing
wireDistanceToPoint (l:ls) p =
  case lineDistanceToPoint l p of
    Nothing -> fmap (lineLength l +) (wireDistanceToPoint ls p)
    Just d -> Just d
  where lineLength Line{start, finish} = distance start finish

minimizedSumDistance :: [Line] -> [Line] -> Int
minimizedSumDistance w1 w2 =
  let intersections = wireIntersections w1 w2
      distances =
        flip mapMaybe intersections $ \p -> do
          d1 <- wireDistanceToPoint w1 p
          d2 <- wireDistanceToPoint w2 p
          pure (d1 + d2)
   in minimum distances

------------------------------------PARSING------------------------------------

parseWireStep :: ([Line], Point) -> Text -> ([Line], Point)
parseWireStep (ls, p@Point{x,y}) t =
  let Just (c, rest) = T.uncons t
      d = read (T.unpack rest) :: Int
      p' = case c of
             'L' -> p { x = x - d }
             'R' -> p { x = x + d }
             'D' -> p { y = y - d }
             'U' -> p { y = y + d }
      l = Line p p'
   in (l:ls, p')

parseWire :: Text -> [Line]
parseWire = reverse . fst . foldl' parseWireStep ([], origin) . T.splitOn ","

parseWirePair :: Text -> ([Line], [Line])
parseWirePair input = let (a:b:_) = T.splitOn "\n" input
                       in (parseWire a, parseWire b)

--------------------------------------MAIN--------------------------------------

main :: IO ()
main = do
  (w1, w2) <- parseWirePair <$> T.readFile "3.txt"
  -- Part one
  print . minimum . filter (>0) . map (distance origin) $ wireIntersections w1 w2
  -- Part two
  print $ minimizedSumDistance w1 w2
