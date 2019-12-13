import Control.Lens hiding (Empty)
import Data.Foldable
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M
import Intcode
import Linear.V2

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
toTiles = M.fromList . map parseTriple . chunksOf 3
  where
    parseTriple [x, y, t] = (V2 x y, toEnum (fromIntegral t))

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
          Just Empty -> " "
          Nothing -> " "
          _ -> "█"
  in unlines $ map row [y1 .. y2]

main = do
  cs <- fromFile "../13.txt"
  part1Cs <- runWithInput [] cs
  let part1 =
        M.size . M.filter (== Block) . toTiles . toList . view outputs $ part1Cs
  print part1
  _ <- getLine
  let csWithQuarters = (code %~ M.insert 0 2) cs
  cs' <- runWithInput [] csWithQuarters
  putStrLn . render . toTiles . toList . view outputs $ cs'
