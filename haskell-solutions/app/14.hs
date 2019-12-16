module Main where

import Control.Arrow
import Control.Lens
import Control.Monad.State
import Data.Attoparsec.Text hiding (count)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Map.Merge.Strict
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Debug.Trace

data Factor = Factor
  { _count :: Int
  , _label :: Text
  } deriving (Show)

makeLenses ''Factor

data Formula = Formula
  { _outputCount :: Int
  , _inputs :: [Factor]
  } deriving (Show)

makeLenses ''Formula

type Digraph = Map Text Formula

parseFactor :: Parser Factor
parseFactor = do
  c <- read <$> many1 digit
  space
  l <- T.pack <$> many1 letter
  pure $ Factor c l

parseFormula :: Parser (Text, Formula)
parseFormula = do
  ins <- parseFactor `sepBy1` string ", "
  string " => "
  Factor c out <- parseFactor
  pure (out, Formula c ins)

parseDigraph :: Parser (Map Text Formula)
parseDigraph = fmap M.fromList (parseFormula `sepBy1` endOfLine)

data WalkState = WalkState
  { _existing :: Map Text Int
  , _digraph :: Digraph
  } deriving (Show)

defaultWalkState :: Digraph -> WalkState
defaultWalkState d = WalkState {_existing = M.empty, _digraph = d}

makeLenses ''WalkState

type Walk a = State WalkState a

divRoundUp :: Int -> Int -> Int
divRoundUp a b
  | r == 0 = q
  | otherwise = q + 1
  where
    (q, r) = a `divMod` b

neededFromFormula :: Int -> Formula -> [(Text, Int)]
neededFromFormula repeatCount f =
  let ins = f ^. inputs
      outputMultiple = f ^. outputCount
      requiredForFactor (Factor inputMultiple l) =
        (l, repeatCount * inputMultiple)
  in map requiredForFactor ins

needed :: Text -> Int -> Walk (Int, Map Text Int)
needed t desiredCount = do
  d <- use digraph
  case M.lookup t d of
    Nothing -> pure (0, M.empty)
    Just formula -> do
      let repeatCount = desiredCount `divRoundUp` outputMultiple
          factors = M.fromList . neededFromFormula repeatCount $ formula
          outputMultiple = formula ^. outputCount
      pure (repeatCount * outputMultiple, factors)

difference :: (Ord a, Ord n, Num n) => Map a n -> Map a n -> Map a n
difference =
  merge
    preserveMissing
    dropMissing
    (zipWithMatched $ \_ l r -> max (l - r) 0)

removeExistingFrom :: Map Text Int -> Walk (Map Text Int)
removeExistingFrom m = do
  e <- use existing
  existing .= e `difference` m
  pure (m `difference` e)

-- If the current target is ore, we're done; return the amount of the target.
-- Otherwise:
--  1. Remove any leftover products from the requirements list
--  2. Walk the the nodes under the current target and recur.
--  3. If we produced more than we needed, add the remainder to _existing.
minOre :: Text -> Int -> Walk Int
minOre "ORE" targetCount = pure targetCount
minOre targetLabel targetCount = do
  (produced, factors) <- needed targetLabel targetCount
  requirements <- removeExistingFrom factors
  e <- use existing
  result <- fmap sum . mapM (uncurry minOre) . M.toList $ requirements
  let leftover = produced - targetCount
  existing %= M.insertWith (+) targetLabel leftover
  pure result

walk :: Walk a -> Digraph -> a
walk w d = evalState w (defaultWalkState d)

main :: IO ()
main = do
  Right digraph <- parseOnly parseDigraph <$> T.readFile "./sample.txt"
  print . walk (minOre "FUEL" 1) $ digraph
