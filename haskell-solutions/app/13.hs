import Control.Concurrent
import Control.Lens hiding (Empty)
import Data.Foldable
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Intcode
import Linear.V2
import System.Console.ANSI

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

getScores :: [Integer] -> [Integer]
getScores = mapMaybe parseTriple . chunksOf 3
  where
    parseTriple [-1, 0, s] = Just s
    parseTriple _ = Nothing

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
          Just Ball -> "●"
          Just Paddle -> "▭"
          Just Block -> "▒"
          Just Wall -> "█"
          _ -> " "
  in unlines $ map row [y1 .. y2]

positionOf :: Tile -> Map a Tile -> a
positionOf t b =
  let Just (p, _) = find ((== t) . snd) . M.toList $ b
  in p

paddlePos :: Board -> Point
paddlePos = positionOf Paddle

ballPos :: Board -> Point
ballPos = positionOf Ball

followBall :: Board -> Integer
followBall board = signum . view _x $ ballPos board - paddlePos board

findHighScore :: ComputerState -> IO (Maybe Integer)
findHighScore cs = do
  let board = toTiles . toList . view outputs $ cs
      nextIn = followBall board
      score = lastMay . getScores . toList . view outputs $ cs
  -- If you want it pretty and slow af:
  clearScreen
  putStr "Score: "
  print (fromMaybe 0 score)
  putStrLn (render board)
  threadDelay 50000
  if cs ^. status == Halted
    then pure score
    else findHighScore =<< runWithInput [nextIn] cs

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay xs = Just (last xs)

main :: IO ()
main = do
  cs <- fromFile "../13.txt"
  part1Cs <- runWithInput [] cs
  let part1 =
        M.size . M.filter (== Block) . toTiles . toList . view outputs $ part1Cs
  print part1
  let csWithQuarters = (code %~ M.insert 0 2) cs
  -- Run it one step so we have a screen to start with
  cs' <- runWithInput [] csWithQuarters
  Just part2 <- findHighScore cs'
  print part2
