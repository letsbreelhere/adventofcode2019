{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module DayThree where

import Control.Applicative (liftA2)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Text (Text)
import Data.List (foldl', minimum)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Point = Point { x :: Int, y :: Int }
  deriving (Show, Eq)

origin = Point 0 0

data Line = Line { start :: Point, finish :: Point }
  deriving (Show, Eq)

parseWireStep :: ([Line], Point) -> Text -> ([Line], Point)
parseWireStep (ls, p) t =
  let Just (c, rest) = T.uncons t
      diff = read (T.unpack rest) :: Int
      p' = case c of
             'L' -> p { x = x p - diff }
             'R' -> p { x = x p + diff }
             'D' -> p { y = y p - diff }
             'U' -> p { y = y p + diff }
      l = Line { start = p, finish = p' }
   in (l:ls, p')

parseWire :: Text -> [Line]
parseWire = reverse . fst . foldl' parseWireStep ([], origin) . T.splitOn ","

parseWirePair :: Text -> ([Line], [Line])
parseWirePair input = let (a:b:_) = T.splitOn "\n" input
                       in (parseWire a, parseWire b)

isVertical :: Line -> Bool
isVertical l = y (start l) /= y (finish l)

isHorizontal :: Line -> Bool
isHorizontal = not . isVertical

between a a' x = let (b, b') = (min a a', max a a')
                  in b <= x && x <= b'

intersect :: Line -> Line -> Maybe Point
intersect l1 l2
  | isVertical l1 && isHorizontal l2 =
    let l1x = x (start l1)
        l2y = y (start l2)
     in if between (x (start l2)) (x (finish l2)) l1x &&
           between (y (start l1)) (y (finish l1)) l2y
           then Just $ Point l1x l2y
           else Nothing
  | isVertical l2 && isHorizontal l1 = l2 `intersect` l1
  | otherwise = Nothing

wireIntersections :: [Line] -> [Line] -> [Point]
wireIntersections ls ls' =
  filter (/= origin) . catMaybes $ liftA2 intersect ls ls'

manhattan Point{ x, y } = abs x + abs y
manhattan :: Point -> Int

lineLength :: Line -> Int
lineLength l
  | isVertical l = abs $ y (finish l) - y (start l)
  | otherwise = abs $ x (finish l) - x (start l)


-- Thanks, I hate it.
lineDistanceToPoint :: Line -> Point -> Maybe Int
lineDistanceToPoint l p =
  let (f, g) = if isVertical l then (x, y) else (y, x)
   in if between (g (start l)) (g (finish l)) (g p) && f p == f (start l)
        then Just $ lineLength (l { finish = p })
        else Nothing

wireDistanceToPoint :: [Line] -> Point -> Maybe Int
wireDistanceToPoint [] _ = Nothing
wireDistanceToPoint (l:ls) p =
  case lineDistanceToPoint l p of
    Nothing -> fmap (lineLength l +) (wireDistanceToPoint ls p)
    Just d -> Just d

minimizedSumDistance :: [Line] -> [Line] -> Int
minimizedSumDistance w1 w2 =
  let intersections = wireIntersections w1 w2
      distances = mapMaybe (\p -> liftA2 (+) (wireDistanceToPoint w1 p) (wireDistanceToPoint w2 p)) intersections
   in minimum distances

main :: IO ()
main = do
  -- Part one
  (w1, w2) <- parseWirePair <$> T.readFile "3.txt"
  print . minimum . filter (>0) . map manhattan $ wireIntersections w1 w2
  -- Part two
  print $ minimizedSumDistance w1 w2
