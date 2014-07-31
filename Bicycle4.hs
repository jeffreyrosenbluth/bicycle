module Bicycle4

  ( Ring (..)
  , Direction (..)
  , Gears
  , Trip (..)
  , Bicycle (..)
  , Ride
  , getTime
  , gears
  , go
  , shift
  , getRPM
  , setRPM
  , eval
  , run
  ) where

import Core
import Control.Applicative
import Control.Monad.RWS
import Text.Printf

getTime :: Action Double
getTime = Wrap $ GetTime Return

gears :: Action (Int, Int)
gears = Wrap $ Gears Return


go :: Double -> Action (Double, Double)
go dist = Wrap $ Go dist Return

shift :: Ring -> Direction -> Action ()
shift r d = Wrap $ Shift r d Return

getRPM :: Action Double
getRPM = Wrap $ GetRPM Return

setRPM :: Double -> Action Double
setRPM x = Wrap $ SetRPM x Return
  
data Free f a
  = Return a 
  | Wrap (f (Free f a))

instance Functor f => Monad (Free f) where
  return = Return

  Return x >>= f = f x
  Wrap m   >>= f = Wrap $ fmap (>>= f) m

instance Functor f => Applicative (Free f) where
  pure = return
  (<*>) = ap

instance Functor f => Functor (Free f) where
  fmap = liftM

data ActionF r
  = GetTime (Double -> r)
  | Gears ((Int, Int) -> r)
  | Go Double ((Double, Double) -> r)
  | Shift Ring Direction (() -> r)
  | GetRPM (Double -> r)
  | SetRPM Double (Double -> r)

instance Functor ActionF where
  fmap f (GetTime g)   = GetTime (f . g)
  fmap f (Gears g)     = Gears (f . g)
  fmap f (Go x g)      = Go x (f . g)
  fmap f (Shift r d g) = Shift r d (f . g)
  fmap f (GetRPM g)    = GetRPM (f . g)
  fmap f (SetRPM x g) = SetRPM x (f . g)

type Action = Free ActionF

run :: Action a -> Ride a
run (Wrap (GetTime f)) = do
  tm <- gets time
  run (f tm)
run (Wrap (Gears f)) = do
  bg <- gets bgGear
  sm <- gets smGear
  run (f (bg, sm))
run (Wrap (Go dist f)) = 
  case f (0,0) of
    (Wrap (Go dist' g)) -> run (Wrap (Go (dist + dist') g))
    _          -> do
      bg <- gets bgGear
      sm <- gets smGear
      r  <- gets rpm
      b  <- ask
      let sp = speed bg sm (wheelDiam b) r
          tm = dist / sp / 60
      tell ["Going: " ++ printf "%.2f" dist ++ " miles at " 
                      ++ printf "%.2f" (sp * 3600) ++ " mph in " 
                      ++ printf "%.2f" tm ++ " minutes."]
      modify (\s -> s {time = time s + tm})
      modify (\s -> s {distance = distance s + dist})
      run (f (sp, tm))
run (Wrap (Shift r d f)) = do
  case (r, d) of
    (Big, Up)     -> bgRingUp
    (Big, Down)   -> bgRingDn
    (Small, Up)   -> smRingUp
    (Small, Down) -> smRingDn
  run (f ())
run (Wrap (GetRPM f)) = do
  c <- gets rpm
  run (f c)
run (Wrap (SetRPM x f)) = do
  tell ["Pedal: Change cadence to " ++ printf "%.2f" x]
  modify (\s -> s {rpm = x})
  run (f x)
run (Return x) = return x
