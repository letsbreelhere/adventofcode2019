module Main where

import Control.Lens ((^.))
import Data.Foldable (toList)
import Intcode

main :: IO ()
main = do
  cs <- fromStdin
  part1 <- runWithInput cs [1]
  print . last . toList $ part1 ^. outputs
  part2 <- runWithInput cs [5]
  print . last . toList $ part2 ^. outputs
