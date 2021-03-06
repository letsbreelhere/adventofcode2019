module Main where

import Control.Lens
       ((%=), (+=), (.=), (.~), (^.), makeLenses, use, uses)
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as Queue
import Data.Sequence ((|>))

import Intcode

data Dir
  = N
  | E
  | W
  | S
  deriving (Show, Enum, Eq)

data Turn
  = L
  | R
  deriving (Show, Enum, Eq)

data Point = P
  { x :: Int
  , y :: Int
  } deriving (Show, Eq, Ord)

data Color
  = Black
  | White
  deriving (Show, Enum, Eq)

type Hull = Map Point Color

data RobotState = RobotState
  { _pos :: Point
  , _hull :: Hull
  , _robotDir :: Dir
  , _computer :: ComputerState
  }

makeLenses ''RobotState

type Robot a = StateT RobotState IO a

turnDir :: Turn -> Dir -> Dir
turnDir L N = W
turnDir L E = N
turnDir L W = S
turnDir L S = E
turnDir R N = E
turnDir R E = S
turnDir R W = N
turnDir R S = W

move :: Dir -> Point -> Point
move N (P x y) = P x (y + 1)
move E (P x y) = P (x + 1) y
move W (P x y) = P (x - 1) y
move S (P x y) = P x (y - 1)

turnAndMove :: Turn -> Robot ()
turnAndMove turn = do
  robotDir %= turnDir turn
  newDir <- use robotDir
  p <- use pos
  pos %= move newDir

colorSquare :: Color -> Robot ()
colorSquare c = do
  p <- use pos
  hull %= M.insert p c

curColor :: Robot Color
curColor = do
  os <- toList <$> use (computer . outputs)
  p <- use pos
  curColor <- uses hull (M.lookup p)
  pure $ fromMaybe Black curColor

loopUntilOutput :: Robot (Maybe Integer)
loopUntilOutput = do
  input <- fromIntegral . fromEnum <$> curColor
  (mout, cs') <- liftIO . runUntilOutput [input] =<< use computer
  computer .= cs'
  case cs' ^. status of
    Halted -> pure Nothing
    AwaitingInput -> do
      computer.status .= Running
      loopUntilOutput
    Running -> pure mout

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:y:xs) = (x, y) : pairs xs

stepRobot :: Robot ()
stepRobot = do
  mc <- loopUntilOutput
  case mc of
    Nothing -> pure ()
    Just color  -> do
      Just turn <- loopUntilOutput
      colorSquare (toEnum . fromIntegral $ color)
      turnAndMove (toEnum . fromIntegral $ turn)

runRobot :: RobotState -> IO RobotState
runRobot = execStateT go
  where
    go = do
      s <- use (computer . status)
      if s == Halted
        then pure ()
        else stepRobot >> go

defaultState :: ComputerState -> RobotState
defaultState = RobotState (P 0 0) M.empty N

bounds :: Hull -> (Point, Point)
bounds h =
  let ps = M.keys h
      maxX = maximum $ map x ps
      maxY = maximum $ map y ps
      minX = minimum $ map x ps
      minY = minimum $ map y ps
  in (P minX minY, P maxX maxY)

showHull :: Hull -> [String]
showHull h =
  let (P x1 y1, P x2 y2) = bounds h
      row ry = do
        rx <- [x1 .. x2]
        case M.lookup (P rx ry) h of
          Just White -> "█"
          _ -> " "
  in map row [y2,y2 - 1 .. y1]

main :: IO ()
main = do
  part1State <- defaultState <$> fromStdin
  part1Result <- runRobot part1State
  print . length . M.keys $ part1Result ^. hull
  let part2State = part1State {_hull = M.fromList [(P 0 0, White)]}
  part2Result <- runRobot part2State
  putStrLn . unlines . showHull $ part2Result ^. hull
