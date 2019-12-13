{-# LANGUAGE ViewPatterns #-}
module Intcode where

import Control.Lens ((%=), (+=), (.=), (.~), (%~), (^.), makeLenses, use)
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Functor
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq, ViewL(..), (|>), (><))
import qualified Data.Sequence as Queue
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Maybe
import Safe

type Queue a = Seq a

push :: a -> Queue a -> Queue a
push = flip (|>)

data Status = Running | Halted | AwaitingInput
  deriving (Show, Eq)

data ComputerState = ComputerState
  { _code :: Map Integer Integer
  , _ip :: Integer
  , _status :: Status
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
  deriving (Show)

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
  , _status = Running
  , _inputs = Queue.empty
  , _outputs = Queue.empty
  , _debugOn = False
  , _relativeBase = 0
  }

fetch :: Integer -> Intcode Integer
fetch i = (fromMaybe 0 . M.lookup i) <$> use code

fetchOffset :: Integer -> Intcode Integer
fetchOffset offset = use ip >>= pure . (+ offset) >>= fetch

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

withIndex :: [a] -> [(Integer, a)]
withIndex = zip [0 ..]

operands :: Intcode [Integer]
operands = do
  len <- parameterLength <$> opcode
  modesWithIndex <- withIndex <$> mode
  forM (take (fromIntegral len) modesWithIndex) $ \(i, mode) -> do
    param <- fetchOffset (i + 1)
    case mode of
      Immediate -> pure param
      Position -> fetch param
      Relative -> do
        r <- use relativeBase
        fetch (r + param)

setResult :: Integer -> Intcode ()
setResult value = do
  len <- parameterLength <$> opcode
  modeList <- mode
  let m = modeList !! fromIntegral (len - 1)
  resultIndex <-
    case m of
      Immediate -> error "Invalid mode for result operand"
      Position -> fetchOffset len
    -- N.B. this assumes the last param is used to reference the result operand.
    -- Could probably be refactored.
      Relative -> do
        r <- use relativeBase
        i <- fetchOffset len
        pure (r + i)
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
        Just inp -> do
          debug ("Read input " ++ show inp)
          setResult inp
        Nothing -> status .= AwaitingInput
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
      in setResult (fromIntegral $ fromEnum (l < r)) $> Nothing
    8 ->
      let (l:r:_) = operands
       in setResult (fromIntegral $ fromEnum (l == r)) $> Nothing
    9 -> do
      let (i:_) = operands
      relativeBase += i
      r <- use relativeBase
      debug ("Relative base is now " ++ show r)
      nop
    99 -> (status .= Halted) $> Nothing

step :: Intcode ()
step = do
  i <- use ip
  oc <- opcode
  inst <- curInstruction
  ops <- operands
  let len = parameterLength oc
  params <- mapM fetchOffset [i + 1 .. i + len]
  debug . concat $
    [ "ip:"
    , show i
    , ", instruction:"
    , show inst
    , ", operands:"
    , show ops
    , ", params:"
    , show params
    ]
  jmp <- exec oc ops
  waiting <- (AwaitingInput ==) <$> use status
  if waiting
     then pure ()
     else case jmp of
            Just ix -> ip .= ix
            Nothing -> ip += (parameterLength oc + 1)

while :: Intcode Bool -> Intcode () -> Intcode ()
while mcond f = do
  cond <- mcond
  if cond
    then f >> while mcond f
    else pure ()

run :: ComputerState -> IO ComputerState
run = execStateT (runIntcode $ while running step) . (status .~ Running)
  where
    running = (== Running) <$> use status

step' :: ComputerState -> IO ComputerState
step' = execStateT (runIntcode step)

stepUntilOutput :: ComputerState -> IO ComputerState
stepUntilOutput = execStateT (runIntcode $ while running step)
  where
    running = do
      r <- (== Running) <$> use status
      noOutput <- null <$> use outputs
      pure $ r && noOutput

runUntilOutput :: [Integer] -> ComputerState -> IO (Maybe Integer, ComputerState)
runUntilOutput (Queue.fromList -> ins) cs = do
  cs' <- stepUntilOutput cs{_inputs=ins}
  pure $ case Queue.viewl (cs' ^. outputs) of
    (x:<_) -> (Just x, cs'{_outputs=Queue.empty})
    _ -> (Nothing, cs')

fromStdin :: IO ComputerState
fromStdin = fromString <$> T.getContents

fromFile :: FilePath -> IO ComputerState
fromFile = fmap fromString . T.readFile

fromString :: Text -> ComputerState
fromString = computerState . M.fromList . withIndex . map (read . T.unpack) . T.splitOn ","

runWithInput :: [Integer] -> ComputerState -> IO ComputerState
runWithInput ins = run . (inputs %~ (>< Queue.fromList ins))
