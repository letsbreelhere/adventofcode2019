module Intcode where

import Control.Lens ((%=), (+=), (.=), (.~), makeLenses, use)
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Functor
import Data.List (intercalate)
import Data.Sequence (Seq, ViewL(..), (|>))
import qualified Data.Sequence as Queue
import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Queue a = Seq a

push :: a -> Queue a -> Queue a
push = flip (|>)

data ComputerState = ComputerState
  { _code :: Map Integer Integer
  , _ip :: Integer
  , _halted :: Bool
  , _waiting :: Bool
  , _inputs :: Queue Integer
  , _outputs :: Queue Integer
  , _debugOn :: Bool
  , _relativeBase :: Integer
  }

makeLenses ''ComputerState

newtype Intcode a = Intcode
  { runIntcode :: StateT ComputerState IO a
  } deriving (Functor, Applicative, Monad, MonadState ComputerState, MonadIO)

data Mode
  = Position
  | Immediate
  | Relative

debug :: String -> Intcode ()
debug s = do
  d <- use debugOn
  if d
    then liftIO (putStrLn s)
    else pure ()

computerState :: Map Integer Integer -> ComputerState
computerState code =
  ComputerState
  { _code = code
  , _ip = 0
  , _halted = False
  , _waiting = False
  , _inputs = Queue.empty
  , _outputs = Queue.empty
  , _debugOn = False
  , _relativeBase = 0
  }

fetch :: Integer -> Intcode Integer
fetch i = (! i) <$> use code

fetchOffset :: Integer -> Intcode Integer
fetchOffset offset = do
  i <- use ip
  fetch (i + offset)

curInstruction :: Intcode Integer
curInstruction = fetchOffset 0

opcode :: Intcode Integer
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
        '2' -> Relative
        d -> error ("Unrecognized mode " ++ [d])

parameterLength :: Integer -> Integer
parameterLength opcode
  | opcode `elem` [1, 2, 7, 8] = 3
  | opcode `elem` [5, 6] = 2
  | opcode `elem` [3, 4, 9] = 1
  | opcode == 99 = 0
  | otherwise = error ("Unknown opcode " ++ show opcode)

withIndex :: [a] -> [(a, Integer)]
withIndex = flip zip [1 ..]

operands :: Intcode [Integer]
operands = do
  len <- parameterLength <$> opcode
  modesWithIndex <- withIndex <$> mode
  forM (take (fromIntegral len) modesWithIndex) $ \(mode, i) -> do
    param <- fetchOffset i
    case mode of
      Immediate -> pure param
      Position -> fetch param
      Relative -> fetch . (param +) =<< use relativeBase

setResult :: Integer -> Intcode ()
setResult value = do
  len <- parameterLength <$> opcode
  resultIndex <- fetchOffset len
  code %= M.insert resultIndex value

readInput :: Intcode (Maybe Integer)
readInput = do
  is <- use inputs
  case Queue.viewl is of
    EmptyL -> pure Nothing
    i :< is' -> do
      inputs .= is'
      pure (Just i)

exec :: Integer -> [Integer] -> Intcode (Maybe Integer)
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
    9 -> let (i:_) = operands in (relativeBase += i) $> Nothing
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

fromStdin :: IO ComputerState
fromStdin =
  fmap (computerState . M.fromList . zip [0..] . map (read . T.unpack) . T.splitOn ",") T.getContents

runWithInput :: ComputerState -> [Integer] -> IO ComputerState
runWithInput cs ins = run . (inputs .~ Queue.fromList ins) $ cs
