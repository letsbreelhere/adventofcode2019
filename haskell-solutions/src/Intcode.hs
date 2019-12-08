{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving,
  LambdaCase #-}

module Intcode where

import Control.Lens ((%=), (+=), (.=), (.~), makeLenses, use)
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Functor
import Data.List (intercalate)
import Data.Sequence (Seq, ViewL(..), (|>))
import qualified Data.Sequence as Queue
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Vector.Generic.Mutable (write)

type Queue a = Seq a

push :: a -> Queue a -> Queue a
push = flip (|>)

data ComputerState = ComputerState
  { _code :: Vector Int
  , _ip :: Int
  , _halted :: Bool
  , _waiting :: Bool
  , _inputs :: Queue Int
  , _outputs :: Queue Int
  , _debugOn :: Bool
  }

makeLenses ''ComputerState

newtype Intcode a = Intcode
  { runIntcode :: StateT ComputerState IO a
  } deriving (Functor, Applicative, Monad, MonadState ComputerState, MonadIO)

data Mode
  = Position
  | Immediate

debug :: String -> Intcode ()
debug s = do
  d <- use debugOn
  if d
    then liftIO (putStrLn s)
    else pure ()

computerState :: Vector Int -> ComputerState
computerState code =
  ComputerState
  { _code = code
  , _ip = 0
  , _halted = False
  , _waiting = False
  , _inputs = Queue.empty
  , _outputs = Queue.empty
  , _debugOn = False
  }

fetch :: Int -> Intcode Int
fetch i = (! i) <$> use code

fetchOffset :: Int -> Intcode Int
fetchOffset offset = do
  i <- use ip
  fetch (i + offset)

curInstruction :: Intcode Int
curInstruction = fetchOffset 0

opcode :: Intcode Int
opcode = fmap (`mod` 100) curInstruction

mode :: Intcode [Mode]
mode = do
  inst <- curInstruction
  let padded = drop 2 . (++ repeat '0') . reverse . show $ inst
  pure $ map toMode padded
  where
    toMode =
      \case
        '0' -> Position
        '1' -> Immediate
        d -> error ("Unrecognized mode " ++ [d])

parameterLength :: Int -> Int
parameterLength opcode
  | opcode `elem` [1, 2, 7, 8] = 3
  | opcode `elem` [5, 6] = 2
  | opcode `elem` [3, 4] = 1
  | opcode == 99 = 0
  | otherwise = error ("Unknown opcode " ++ show opcode)

withIndex :: [a] -> [(a, Int)]
withIndex = flip zip [1 ..]

operands :: Intcode [Int]
operands = do
  len <- parameterLength <$> opcode
  modesWithIndex <- withIndex <$> mode
  forM (take len modesWithIndex) $ \(mode, i) -> do
    param <- fetchOffset i
    case mode of
      Immediate -> pure param
      Position -> fetch param

setResult :: Int -> Intcode ()
setResult value = do
  len <- parameterLength <$> opcode
  resultIndex <- fetchOffset len
  code %= V.modify (\v -> write v resultIndex value)

readInput :: Intcode (Maybe Int)
readInput = do
  is <- use inputs
  case Queue.viewl is of
    EmptyL -> pure Nothing
    i :< is' -> do
      inputs .= is'
      pure (Just i)

exec :: Int -> [Int] -> Intcode (Maybe Int)
exec opcode operands = do
  let nop = pure Nothing
      jumpTo = pure . Just
  case opcode of
    1 ->
      let (l:r:_) = operands
      in setResult (l + r) $> Nothing
    2 ->
      let (l:r:_) = operands
      in setResult (l * r) $> Nothing
    3 -> do
      readInput >>= \case
        Just inp -> setResult inp
        Nothing -> (waiting .= True)
      nop
    4 ->
      let (o:_) = operands
      in (outputs %= push o) $> Nothing
    5 ->
      let (v:ix:_) = operands
      in if v /= 0
           then jumpTo ix
           else nop
    6 ->
      let (v:ix:_) = operands
      in if v == 0
           then jumpTo ix
           else nop
    7 ->
      let (l:r:_) = operands
      in setResult
           (if l < r
              then 1
              else 0) $>
         Nothing
    8 ->
      let (l:r:_) = operands
      in setResult
           (if l == r
              then 1
              else 0) $>
         Nothing
    99 -> (halted .= True) $> Nothing

step :: Intcode ()
step = do
  oc <- opcode
  debug . concat $ ["opcode:", show oc]
  ops <- operands
  debug . concat $ ["operands:", show ops]
  jmp <- exec oc ops
  case jmp of
    Just ix -> ip .= ix
    Nothing -> ip += (parameterLength oc + 1)

while :: Intcode Bool -> Intcode () -> Intcode ()
while mcond f = do
  cond <- mcond
  if cond
    then f >> while mcond f
    else pure ()

run :: ComputerState -> IO ComputerState
run cs = do
  (_, cs') <- runStateT (runIntcode $ while (not <$> use halted) step) cs
  pure cs'

runWithInput :: ComputerState -> [Int] -> IO ComputerState
runWithInput cs ins = run . (inputs .~ Queue.fromList ins) $ cs
