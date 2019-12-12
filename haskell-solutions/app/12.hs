module Main where

import Control.Lens
import Data.Maybe
import Data.Foldable
import Data.List
import Linear.V3
import Data.Set (Set)
import Debug.Trace
import qualified Data.Set as S

type Point = V3 Int
data Moon = Moon
  { _pos :: Point
  , _vel :: Point
  } deriving (Show)
makeLenses ''Moon

mkMoon :: Point -> Moon
mkMoon p = Moon p (V3 0 0 0)

velocityDeltaAt :: Int -> [Moon] -> Point
velocityDeltaAt i ms =
  let m = ms !! i
      ms' = take i ms ++ drop (i+1) ms
      deltas = map (\m' -> signum (m'^.pos - m^.pos)) ms'
   in sum deltas

step :: [Moon] -> [Moon]
step ms =
  let ds = map (`velocityDeltaAt` ms) [0..length ms]
      ms' = zipWith (\m d -> m & vel +~ d) ms ds
   in map (\m -> let v = m^.vel in m & pos +~ v) ms'

energy :: Moon -> Int
energy (Moon p v) = mag p * mag v
  where mag = sum . toList . abs

input :: [Moon]
input =
  [ mkMoon (V3 3 (-6) 6)
  , mkMoon (V3 10 7 (-9))
  , mkMoon (V3 (-3) (-7) 9)
  , mkMoon (V3 (-8) 0 4)
  ]

atStep :: [Moon] -> Int -> [Moon]
atStep ms s = iterate step ms !! s

-- This should be a stream but I'm feeling lazy.
periodOf :: Ord a => [a] -> (a, Int)
periodOf = go S.empty 0
  where go m n (x:xs) = (if n `mod` 10000 == 0 then traceShow n else id) $
          if S.member x m
            then (x, n)
            else go (S.insert x m) (n+1) xs

test :: [Moon]
test =
  [ mkMoon (V3 (-1) 0 2)
  , mkMoon (V3 2 (-10) (-7))
  , mkMoon (V3 4 (-8) 8)
  , mkMoon (V3 3 5 (-1))
  ]

main :: IO ()
main = do
  let stepped = input `atStep` 1000
      part1 = sum . map energy $ stepped
  print part1
  let ss = iterate step input
      getX m = (m^.vel._x, m^.pos._x)
      getY m = (m^.vel._y, m^.pos._y)
      getZ m = (m^.vel._z, m^.pos._z)

      xs = map (map getX) ss
      (xp, px) = periodOf xs
  print (xp, px)

  let ys = map (map getY) ss
      (yp, py) = periodOf ys
  print (yp, py)

  let zs = map (map getZ) ss
      (zp, pz) = periodOf zs
  print (zp, pz)

  let Just firstX = elemIndex xp xs
      Just firstY = elemIndex yp ys
      Just firstZ = elemIndex zp zs
      l = foldr lcm 1 [px, py, pz]
  print l
