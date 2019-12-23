module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (sortOn, foldl', elemIndex)
import Data.Attoparsec.Text (Parser, string, signed, decimal, parseOnly, sepBy1, char)
import Control.Applicative

type Deck = [Int]

data Move = DealIntoNew
          | Cut Int
          | DealWith Int
          deriving (Show)

dealWith :: Int -> Deck -> Deck
dealWith n d =
  let indices = map (\i -> (i*n) `mod` length d) [0..length d - 1]
   in map fst . sortOn snd $ d `zip` indices

performMove :: Move -> Deck -> Deck
performMove DealIntoNew d = reverse d
performMove (Cut n) d
  | n > 0 = drop n d ++ take n d
  | otherwise = drop (length d + n) d ++ take (length d + n) d
performMove (DealWith n) d = dealWith n d

perform :: Int -> [Move] -> Deck
perform n = foldl' (flip performMove) (defaultDeck n)

defaultDeck :: Int -> Deck
defaultDeck len = [0..len-1]

parseMoves :: Text -> Either String [Move]
parseMoves = parseOnly parseMoves'

parseMoves' :: Parser [Move]
parseMoves' = parseMove `sepBy1` char '\n'

parseMove :: Parser Move
parseMove =
  parseDealNew <|> parseCut <|> parseDealWith

parseDealNew = string "deal into new stack" *> pure DealIntoNew
parseCut = do
  string "cut "
  n <- signed decimal
  pure (Cut n)
parseDealWith = do
  string "deal with increment "
  n <- signed decimal
  pure (DealWith n)

main :: IO ()
main = do
  Right input <- parseMoves <$> T.readFile "../22.txt"
  print (elemIndex 2019 $ perform 10007 input)
