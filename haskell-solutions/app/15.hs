module Main where

import Intcode
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Linear.V2
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Lens
import System.IO
import Data.Set (Set)
import qualified Data.Set as S

data Dir = N | S | W | E
  deriving (Show, Eq, Ord)

oppose :: Dir -> Dir
oppose N = S
oppose S = N
oppose E = W
oppose W = E

getKey :: IO String
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

getDir :: IO Dir
getDir = do
  k <- getKey
  case k of
    "\ESC[A" -> pure N
    "\ESC[B" -> pure S
    "\ESC[C" -> pure E
    "\ESC[D" -> pure W
    _        -> fail $ "Unexpected input " ++ show k

instance Enum Dir where
  fromEnum N = 1
  fromEnum S = 2
  fromEnum W = 3
  fromEnum E = 4
  toEnum 1 = N
  toEnum 2 = S
  toEnum 3 = W
  toEnum 4 = E

data Tile = Wall | Floor | Goal
  deriving (Show, Eq, Enum)

data Turn
  = L
  | R
  deriving (Show, Eq)

type Point = V2 Int

type Layout = Map Point Tile

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
move N (V2 x y) = V2 x (y + 1)
move E (V2 x y) = V2 (x + 1) y
move W (V2 x y) = V2 (x - 1) y
move S (V2 x y) = V2 x (y - 1)

data DroidState = DroidState
  { _pos :: Point
  , _layout :: Layout
  , _computer :: ComputerState
  }
makeLenses ''DroidState

type Droid a = StateT DroidState IO a

defaultState :: ComputerState -> DroidState
defaultState = DroidState 0 (M.fromList [(0, Floor)])

moveDroid :: Dir -> Droid Tile
moveDroid dir = do
  (o, computer') <- liftIO . runUntilOutput [fromIntegral $ fromEnum dir] =<< use computer
  let t = toEnum . fromIntegral . fromJust $ o
  computer .= computer'
  pos' <- move dir <$> use pos
  case t of
    Wall -> pure ()
    _ -> pos .= pos'
  layout %= M.insert pos' t
  pure t

runDroid :: Droid a -> ComputerState -> IO (a, DroidState)
runDroid p = runStateT p . defaultState

bounds :: Layout -> (Point, Point)
bounds l =
  let ps = M.keys l
      maxX = maximum $ map (view _x) ps
      maxY = maximum $ map (view _y) ps
      minX = minimum $ map (view _x) ps
      minY = minimum $ map (view _y) ps
  in (V2 minX minY, V2 maxX maxY)

showLayout :: DroidState -> String
showLayout ds =
  let h = view layout ds
      (V2 x1 y1, V2 x2 y2) = bounds h
      row ry = do
        rx <- [x1 .. x2]
        if V2 rx ry == (ds ^. pos)
           then "@"
           else case M.lookup (V2 rx ry) h of
                  Just Wall -> "█"
                  Just Goal -> "!"
                  Just Floor -> " "
                  Nothing -> "▒"
  in unlines $ map row [y2,y2-1..y1]

instance Show DroidState where
  show = showLayout

repl :: Droid ()
repl = forever $ do
  availableNeighbors
  ds <- get
  liftIO . putStrLn . showLayout $ ds
  dir <- liftIO getDir
  moveDroid dir

availableNeighbors :: Droid [Dir]
availableNeighbors = flip filterM [N, S, E, W] $ \d -> do
  t <- moveDroid d
  case t of
    Wall -> pure False
    _ -> do
      moveDroid (oppose d)
      pure True

backtrack :: [Dir] -> Droid ()
backtrack [] = pure ()
backtrack (d:ds) = do
  moveDroid (oppose d)
  backtrack ds

dfs :: Set Point -> Droid (Set Point)
dfs s = do
  liftIO . print =<< get
  possible <- availableNeighbors
  p <- use pos
  ss <- forM possible $ \d ->
               if S.member p s
                  then pure s
                  else do moveDroid d
                          s' <- dfs (S.insert p s)
                          backtrack [d]
                          pure s'
  let s' = foldr S.union S.empty ss
  pure s'

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  cs <- fromFile "../15.txt"
  runDroid (dfs S.empty) cs
  pure ()
