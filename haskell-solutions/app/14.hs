module Main where

import Control.Arrow
import Control.Lens
import Control.Monad.State
import Data.Attoparsec.Text hiding (count)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T

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
  { _target :: Factor
  , _existing :: Map Text Int
  , _digraph :: Digraph
  }
  deriving (Show)

defaultWalkState :: Digraph -> WalkState
defaultWalkState d =
  WalkState
  { _target = Factor 1 "FUEL"
  , _existing = M.empty
  , _digraph = d
  }
makeLenses ''WalkState

type Walk a = State WalkState a

needed :: Walk (Int, Map Text Int)
needed = do
  Factor multiplier t <- use target
  d <- use digraph
  case M.lookup t d of
    Nothing -> pure (0, M.empty)
    Just formula -> do
      let factors = M.fromList . map ((view label &&& view count) . (count *~ multiplier)) . view inputs $ formula
      pure (formula^.outputCount, factors)

removeExistingFrom :: Map Text Int -> Walk (Map Text Int)
removeExistingFrom = undefined

-- If the current target is ore, we're done; return the amount of the target.
-- Otherwise:
--  1. If we have any leftover elements that apply to the current requirements,
--     they annihilate one another.
--  2. Walk the the nodes under the current target and recur.
--  3. If we produced more than we needed, add the remainder to _existing.
minOre :: Walk Int
minOre = do
  t <- use target
  if t^.label == "ORE"
     then pure (t^.count)
     else do
       (produced, factors) <- needed
       requirements <- removeExistingFrom factors
       result <- fmap sum . mapM minOreAt . M.toList $ requirements
       let leftover = produced - t^.count
       if leftover < 0
          then fail "Didn't produce enough products"
          else pure ()
       existing %= M.insertWith (+) (t^.label) leftover
       pure result

minOreAt :: (Text, Int) -> Walk Int
minOreAt (l, c) = do
  target .= Factor c l
  minOre

walk :: Walk a -> Digraph -> a
walk w d = evalState w (defaultWalkState d)

main :: IO ()
main = do
  Right digraph <- parseOnly parseDigraph <$> T.readFile "sample.txt"
  print . walk minOre $ digraph
