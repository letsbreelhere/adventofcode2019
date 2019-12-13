import Control.Lens hiding (Empty)
import Data.Foldable
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Intcode
import Linear.V2
import System.Console.ANSI
import Control.Concurrent

data Tile
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Show, Enum, Eq)

type Point = V2 Integer

type Board = Map Point Tile

toTiles :: [Integer] -> Board
toTiles = M.fromList . mapMaybe parseTriple . chunksOf 3
  where
    parseTriple [x, y, t]
      | x >= 0 = Just (V2 x y, toEnum (fromIntegral t))
      | otherwise = Nothing

bounds :: Board -> (Point, Point)
bounds b =
  let ps = M.keys b
      maxX = maximum $ map (view _x) ps
      maxY = maximum $ map (view _y) ps
      minX = minimum $ map (view _x) ps
      minY = minimum $ map (view _y) ps
  in (V2 minX minY, V2 maxX maxY)

render :: Board -> String
render board =
  let (V2 x1 y1, V2 x2 y2) = bounds board
      row ry = do
        rx <- [x1 .. x2]
        case M.lookup (V2 rx ry) board of
          Just Ball -> "o"
          Just Paddle -> "="
          Just Block -> "█"
          _ -> " "
  in unlines $ map row [y1 .. y2]

paddlePos :: Board -> Point
paddlePos b =
  let Just (p, _) = find ((== Paddle) . snd) . M.toList $ b
   in p

ballPos :: Board -> Point
ballPos b =
  let Just (p, _) = find ((== Ball) . snd) . M.toList $ b
   in p

followBall :: Board -> Integer
followBall board =
  let V2 bx by = ballPos board
      V2 px py = paddlePos board
   in signum (bx - px)

getScores :: [Integer] -> [Integer]
getScores = mapMaybe parseTriple . chunksOf 3
  where parseTriple [-1, 0, s] = Just s
        parseTriple _ = Nothing

go :: ComputerState -> IO ()
go cs = do
  let board = toTiles . toList . view outputs $ cs
      scores = getScores . toList . view outputs $ cs
      nextIn = followBall board
      blocksLeft = M.size . M.filter (== Block) . toTiles . toList . view outputs $ cs
  if not $ null scores then print (last scores) else pure ()
  if cs^.status == Halted
     then pure ()
     else do
            cs' <- runWithInput [nextIn] cs
            go cs'

main = do
  cs <- fromFile "../13.txt"
  part1Cs <- runWithInput [] cs
  let part1 =
        M.size . M.filter (== Block) . toTiles . toList . view outputs $ part1Cs
  print part1
  let csWithQuarters = (code %~ M.insert 0 2) cs
  cs' <- runWithInput [] csWithQuarters
  go cs'
