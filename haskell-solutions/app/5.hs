module Main where

import Control.Lens ((^.))
import Data.Foldable (toList)
import Intcode

main :: IO ()
main = do
  cs <- fromStdin
  part1 <- runWithInput [1] cs
  print . last . toList $ part1 ^. outputs
  part2 <- runWithInput [5] cs
  print . last . toList $ part2 ^. outputs
