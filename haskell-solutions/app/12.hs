module Main where

import Control.Lens
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V3

type Point = V3 Int

data Moon = Moon
  { _pos :: Point
  , _vel :: Point
  } deriving (Show)

makeLenses ''Moon

type Universe = [Moon]

mkMoon :: Point -> Moon
mkMoon p = Moon p (V3 0 0 0)

velocityDeltaAt :: Moon -> Universe -> Point
velocityDeltaAt m = sum . map (\m' -> signum (m' ^. pos - m ^. pos))

step :: Universe -> Universe
step ms =
  let ds = map (`velocityDeltaAt` ms) ms
      ms' = zipWith (\m d -> m & vel +~ d) ms ds
  in map
       (\m ->
          let v = m ^. vel
          in m & pos +~ v)
       ms'

energy :: Moon -> Int
energy (Moon p v) = mag p * mag v
  where
    mag = sum . toList . abs

input :: Universe
input =
  [ mkMoon (V3 3 (-6) 6)
  , mkMoon (V3 10 7 (-9))
  , mkMoon (V3 (-3) (-7) 9)
  , mkMoon (V3 (-8) 0 4)
  ]

atStep :: Universe -> Int -> Universe
atStep ms s = iterate step ms !! s

-- This should be a stream but I'm feeling lazy.
periodOf :: Ord a => [a] -> Int
periodOf = go S.empty 0
  where
    go m n (x:xs) =
      if S.member x m
        then n
        else go (S.insert x m) (n + 1) xs

stepAxes :: Getting Int (V3 Int) Int -> [[(Int, Int)]]
stepAxes l = map (map (get l)) (iterate step input)
  where
    get l m = (m ^. vel . l, m ^. pos . l)

main :: IO ()
main = do
  let stepped = input `atStep` 1000
      part1 = sum . map energy $ stepped
  print part1
  let px = periodOf (stepAxes _x)
      py = periodOf (stepAxes _y)
      pz = periodOf (stepAxes _z)
  print (foldr lcm 1 [px, py, pz])
