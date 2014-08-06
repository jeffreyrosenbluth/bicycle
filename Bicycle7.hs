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
getTime = singleton GetTime

gears :: Action (Int, Int)
gears = singleton Gears

go :: Double -> Action (Double, Double)
go = singleton . Go

shift :: Ring -> Direction -> Action ()
shift r d = singleton $ Shift r d

getRPM :: Action Double
getRPM = singleton  GetRPM

setRPM :: Double -> Action Double
setRPM = singleton . SetRPM 
  
data ActionO :: * -> * where
  GetTime :: ActionO Double
  Gears :: ActionO (Int, Int)
  Go :: Double -> ActionO (Double, Double)
  Shift :: Ring -> Direction -> ActionO ()
  GetRPM :: ActionO Double
  SetRPM :: Double -> ActionO Double

type Action = Program ActionO

-- data Program instr a where --
data Program :: (* -> *) -> * -> * where
  Return :: a -> Program instr a
  Bind   :: Program instr a -> (a -> Program instr b) -> Program instr b
  Instr  :: instr a -> Program instr a

instance Monad (Program instr) where
  return = Return
  (>>=)  = Bind

instance Functor (Program instr) where
  fmap = liftM

instance Applicative (Program instr) where
  pure  = return
  (<*>) = ap

singleton :: instr a -> Program instr a
singleton = Instr

run :: Action a -> Ride a
run (Return x) = return x
run (Instr x) = run ((Instr x) `Bind` Return)
run ((m `Bind` g) `Bind` h) = run $ m `Bind` (\x -> g x `Bind` h)
run ((Return x) `Bind` k) = run (k x)
run ((Instr GetTime) `Bind` k) = do
  tm <- gets time
  run (k tm)
run ((Instr Gears) `Bind` k) = do
  bg <- gets bgGear
  sm <- gets smGear
  run (k (bg, sm))
run ((Instr (Go dist)) `Bind` k) = do
  case k (0,0) of
    (Instr (Go dist') `Bind` k') ->
      run (go (dist + dist') >>= k')
    _                -> do
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
      run (k (sp, tm))
run ((Instr (Shift r d)) `Bind` k) = do
  case (r, d) of
    (Big, Up)     -> bgRingUp
    (Big, Down)   -> bgRingDn
    (Small, Up)   -> smRingUp
    (Small, Down) -> smRingDn
  run (k ())
run ((Instr GetRPM) `Bind` k) = do
  c <- gets rpm
  run (k c)
run ((Instr (SetRPM x)) `Bind` k) = do
  tell ["Pedal: Change cadence to " ++ printf "%.2f" x]
  modify (\s -> s {rpm = x})
  run (k x)
