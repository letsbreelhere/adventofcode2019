module Main where

import Data.List
import Control.Monad

factorsForPosition :: Int -> [Int]
factorsForPosition ix = tail . cycle . concatMap (replicate (ix+1)) $ [0,1,0,-1]

applyFft :: [Int] -> [Int]
applyFft is = map ((`mod` 10) . abs . sum . zipWith (*) is . factorsForPosition) [0..length is-1]

applyFft' :: [Int] -> [Int]
applyFft' is = map (sum . zipWith (*) is . factorsForPosition) [0..length is-1]

cumsum :: Num a => [a] -> [a]
cumsum = scanr1 (+)

main :: IO ()
main = do
  digits <- map (read . (:[])) . init <$> readFile "../16.txt" :: IO [Int]
  let part1 = concatMap (show . (`mod` 10)) . take 8 . (!! 100) . iterate applyFft $ digits
  putStrLn part1
  let toSkip = read . concatMap show $ take 7 digits :: Int
      startIndex = toSkip `mod` length digits
      lengthFromSkip = 10000 * length digits - toSkip
      realInput = take lengthFromSkip (drop startIndex digits ++ cycle digits)
      applied = iterate cumsum realInput
  putStrLn . concatMap (show . (`mod` 10) . abs) . take 8 . (!! 100) $ applied

-- After applying FFT `iters` times, find the factor for coefficient `coeff` in
-- the `term`th term.
f 1 0 _ = 0
f coeff term iters
  | coeff < term = 0
  | coeff == term = 1
  | coeff == term + 1 = iters
  | otherwise = (!! term) . (!! iters) . iterate applyFft' $ replicate coeff 0 ++ [1]
