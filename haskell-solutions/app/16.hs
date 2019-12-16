module Main where

import Data.List
import Control.Monad

factorsForPosition :: Int -> [Int]
factorsForPosition ix = tail . cycle . concatMap (replicate (ix+1)) $ [0,1,0,-1]

applyFft :: [Int] -> [Int]
applyFft is = map ((`mod` 10) . abs . sum . zipWith (*) is . factorsForPosition) [0..length is-1]

cumsum :: Num a => [a] -> [a]
cumsum = scanr1 (+)

main :: IO ()
main = do
  digits <- map (read . (:[])) . init <$> readFile "../16.txt" :: IO [Int]
  let part1 = concatMap show . take 8 . (!! 100) . iterate applyFft $ digits
  putStrLn part1
  let toSkip = read . concatMap show $ take 7 digits :: Int
      startIndex = toSkip `mod` length digits
      lengthFromSkip = 10000 * length digits - toSkip
      realInput = take lengthFromSkip (drop startIndex digits ++ cycle digits)
      applied = iterate (map (`mod` 10) . cumsum) realInput
  putStrLn . concatMap show . take 8 . (!! 100) $ applied

newtype V a = V { getV :: [a] }

instance Functor V where
  fmap f (V xs) = V (map f xs)

instance (Eq a, Num a, Show a) => Show (V a) where
  show = intercalate " + " . showV 1 . getV

combine f (V l) (V r) =
    let len = max (length l) (length r)
        l' = l ++ replicate (len - length l ) 0
        r' = r ++ replicate (len - length r ) 0
     in V $ zipWith f l' r'

instance Num a => Num (V a) where
  (+) = combine (+)
  (*) = combine (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger 0 = V []

getCoeff :: Num a => Int -> V a -> a
getCoeff n (V cs) = if length cs > n then cs !! n else 0

applyFft' :: [V Int] -> [V Int]
applyFft' is = map go [0..length is-1]
  where go ix = sum . zipWith scalarMult is . factorsForPosition $ ix
        scalarMult v i = fmap (*i) v

xn :: Num a => Int -> V a
xn n = V $ replicate (n-1) 0 ++ [1]

unit :: Int -> [V Int]
unit n = map xn [1..n]

f coeff term iters len = getCoeff (coeff-1) . (!! (term-1)) . (!! iters) . iterate applyFft' $ unit len

showV :: (Show a, Num a, Eq a) => a -> [a] -> [String]
showV _ [] = []
showV n (0:ks) = showV (n+1) ks
showV n (k:ks) = (coeff k ++ "x_" ++ show n) : showV (n+1) ks
  where coeff 1 = ""
        coeff n = show n
