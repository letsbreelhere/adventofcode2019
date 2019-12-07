{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (intersect)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Safe (headMay)

type Tree = Map Text Text

parseTree :: Text -> Tree
parseTree = M.fromList . map ((\[p, c] -> (c, p)) . T.splitOn ")") . T.lines

parentsOf :: Tree -> Text -> [Text]
parentsOf t s =
  case M.lookup s t of
    Nothing -> []
    Just p -> p : parentsOf t p

heightTo :: Tree -> Text -> Text -> Maybe Int
heightTo t root child
  | root == child = Just 0
  | otherwise = fmap (1 +) (heightTo t root =<< M.lookup child t)

leastUpperBound :: Tree -> Text -> Text -> Maybe Text
leastUpperBound t l r = headMay (parentsOf t l `intersect` parentsOf t r)

distanceBetween :: Tree -> Text -> Text -> Maybe Int
distanceBetween t l r = do
  lub <- leastUpperBound t l r
  (+) <$> heightTo t lub l <*> heightTo t lub r

main :: IO ()
main = do
  t <- parseTree <$> T.readFile "6.txt"
  -- Part 1
  print . sum . map (length . parentsOf t) $ M.keys t
  -- Part 2
  print . fmap (subtract 2) $ distanceBetween t "YOU" "SAN"
