module Main where

import Control.Lens ((^.))
import Data.Foldable (toList)
import Intcode

main :: IO ()
main = do
  cs <- fromStdin
  part1 <- runWithInput cs [1]
  print . head . toList $ part1 ^. outputs
  part2 <- runWithInput cs [2]
  print . head . toList $ part2 ^. outputs
