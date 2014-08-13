{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Bicycle7

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
getTime = liftAct GetTime

gears :: Action (Int, Int)
gears = liftAct Gears

go :: Double -> Action (Double, Double)
go = liftAct . Go

shift :: Ring -> Direction -> Action ()
shift r d = liftAct $ Shift r d

getRPM :: Action Double
getRPM = liftAct  GetRPM

setRPM :: Double -> Action Double
setRPM = liftAct . SetRPM 
  
data ActionO :: * -> * where
  GetTime :: ActionO Double
  Gears :: ActionO (Int, Int)
  Go :: Double -> ActionO (Double, Double)
  Shift :: Ring -> Direction -> ActionO ()
  GetRPM :: ActionO Double
  SetRPM :: Double -> ActionO Double

type Action = Program ActionO

data Program :: (* -> *) -> * -> * where
  Return :: a -> Program instr a
  Bind   :: instr a -> (a -> Program instr b) -> Program instr b

instance Monad (Program instr) where
  return = Return
  -- left identity
  Return a >>= k = k a
  -- associativity
  (Bind ia f) >>= k = Bind ia (\a -> f a >>= k)

instance Functor (Program instr) where
  fmap = liftM

instance Applicative (Program instr) where
  pure  = return
  (<*>) = ap

-- right identity
liftAct :: t a -> Program t a
liftAct ta = Bind ta Return

run :: Action a -> Ride a
run (Return x) = return x
run (Bind (Go dist) k) =
  case k (0, 0) of
    Bind (Go dist') k' -> run (Bind (Go (dist + dist')) k')
    _                  -> runAct (Go dist) >>= \x -> run (k x)
run (Bind ix k) = runAct ix >>= \x -> run (k x)

runAct :: ActionO a -> Ride a
runAct GetTime = gets time
runAct Gears = do
  bg <- gets bgGear
  sm <- gets smGear
  return (bg, sm)
runAct (Go dist) = do
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
  return (sp, tm)
runAct (Shift r d) = case (r, d) of
  (Big, Up)     -> bgRingUp
  (Big, Down)   -> bgRingDn
  (Small, Up)   -> smRingUp
  (Small, Down) -> smRingDn
runAct GetRPM = gets rpm
runAct (SetRPM x) = do
  tell ["Pedal: Change cadence to " ++ printf "%.2f" x]
  modify (\s -> s {rpm = x})
  return x
