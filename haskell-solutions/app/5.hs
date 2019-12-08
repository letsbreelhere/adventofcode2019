{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.))
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Intcode

getInput :: IO (Vector Int)
getInput =
  fmap (V.fromList . map (read . T.unpack) . T.splitOn ",") T.getContents

main :: IO ()
main = do
  cs <- computerState <$> getInput
  part1 <- runWithInput cs [1]
  print . last . toList $ part1 ^. outputs
  part2 <- runWithInput cs [5]
  print . last . toList $ part2 ^. outputs
