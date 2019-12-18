module Dir where

import Linear.V2

type Point = V2 Int

data Dir
  = N
  | S
  | W
  | E
  deriving (Show, Eq, Ord)

data Turn
  = L
  | R
  deriving (Show, Eq, Ord)

oppose :: Dir -> Dir
oppose N = S
oppose S = N
oppose E = W
oppose W = E

turnDir :: Turn -> Dir -> Dir
turnDir L N = W
turnDir L E = N
turnDir L W = S
turnDir L S = E
turnDir R N = E
turnDir R E = S
turnDir R W = N
turnDir R S = W

turnBetween :: Dir -> Dir -> Maybe Turn
turnBetween N E = Just R
turnBetween E N = Just L

turnBetween E S = Just R
turnBetween S E = Just L

turnBetween S W = Just R
turnBetween W S = Just L

turnBetween W N = Just R
turnBetween N W = Just L

turnBetween _ _ = Nothing

turns :: Dir -> [Dir] -> [Maybe Turn]
turns _ [] = []
turns start (d:ds) =
  let d' = turnBetween start d
   in d' : turns d ds

vectorRep :: Dir -> Point
vectorRep N = V2 0 1
vectorRep S = V2 0 (-1)
vectorRep E = V2 1 0
vectorRep W = V2 (-1) 0

moveDir :: Point -> Dir -> Point
moveDir p d = p + vectorRep d
