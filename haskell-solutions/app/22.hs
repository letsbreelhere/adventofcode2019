module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (sortOn, foldl', elemIndex)
import Data.Attoparsec.Text (Parser, string, signed, decimal, parseOnly, sepBy1, char)
import Control.Applicative ((<|>))

data Move = DealIntoNew
          | Cut Integer
          | DealWith Integer
          deriving (Show)

performAtIndex :: Move -> Integer -> Integer -> Integer
performAtIndex move ix m =
  let res = case move of
              DealIntoNew -> -(ix + 1)
              Cut k -> ix - k
              DealWith k -> ix * k
   in res `mod` m

performAllAtIndex :: [Move] -> Integer -> Integer -> Integer
performAllAtIndex moves initialIx deckSize =
  foldl' (\ix move -> performAtIndex move ix deckSize) initialIx moves

-------------
-- PARSING --
-------------

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

-----------------------------------------
-- TEST FUNCTIONS (unused in solution) --
-----------------------------------------

type Deck = [Integer]

shuffle :: Integer -> Deck -> [Move] -> Deck
shuffle iterations deck moves =
  let len = fromIntegral (length deck)
      (scale, shift) = coefficients moves len
      (a, b) = nthCoeff scale shift iterations len
   in map (\ix -> ((ix - b) * modInv a len)`mod`len) deck

invertMoves :: Integer -> [Move] -> [Move]
invertMoves len = map (invert len) . reverse

invert :: Integer -> Move -> Move
invert _ DealIntoNew = DealIntoNew
invert _ (Cut k) = Cut (-k)
invert n (DealWith k) = DealWith (modInv k n)

---------------
-- SHUFFLING --
---------------

-- Thanks, Rosetta Code.
-- Modular multiplicative inverse (assuming modulus is prime, since it is in
-- this problem, to avoid annoying Maybes).
modInv :: Integer -> Integer -> Integer
modInv a m = mkPos i
  where
    (i, _, _) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

coefficients :: [Move] -> Integer -> (Integer, Integer)
coefficients moves deckSize =
  let shift = performAllAtIndex moves 0 deckSize
      atOne = performAllAtIndex moves 1 deckSize
      scale = atOne - shift
   in (scale `mod` deckSize, shift `mod` deckSize)

-- Thanks again, Rosetta.
modExp' :: Integer -> Integer -> Integer -> Integer -> Integer
modExp' b 0 m r = r
modExp' b e m r
  | e `mod` 2 == 1 = modExp' (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
modExp' b e m r = modExp' (b * b `mod` m) (e `div` 2) m r

modExp :: Integer -> Integer -> Integer -> Integer
modExp b e m | e < 0 = modInv (modExp b (-e) m) m
modExp b e m = modExp' b e m 1

-- Given a, b, x0, m, and n, compute the nth iteration of x_(k+1) = (a*x_k + b) mod m.
-- This is equal (mod m) to:
-- a^n*x_0 + sum(a^k, k <- 0..n-1) * b.
-- == a^n*x_0 + b * ((a^n - 1) / (a - 1))
--         a       -> b       -> x0      -> n       -> m       -> result
nthCoeff a b n m =
  let e = modExp a n m
      rsum = if a == 1 then n + 1 else (e-1) * modInv (a-1) m
   in (e, (b * rsum) `mod` m)

main :: IO ()
main = do
  Right moves <- parseMoves <$> T.readFile "../22.txt"
  let smallDeckSize = 10007
      (scale, shift) = coefficients moves smallDeckSize
      part1 = (scale*2019 + shift) `mod` smallDeckSize
  print part1

  let hugeDeckSize = 119315717514047
      iterations = 101741582076661
      (scale', shift') = coefficients moves hugeDeckSize
      (a, b) = nthCoeff scale' shift' iterations hugeDeckSize
      part2 = (2020 - b) * modInv a hugeDeckSize
  print $ part2 `mod` hugeDeckSize
