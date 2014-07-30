{-# LANGUAGE GADTs                       #-}
{-# LANGUAGE KindSignatures              #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Bicycle2

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
getTime = GetTime

gears :: Action (Int, Int)
gears = Gears

go :: Double -> Action (Double, Double)
go = Go 

shift :: Ring -> Direction -> Action ()
shift = Shift

getRPM :: Action Double
getRPM = GetRPM

setRPM :: Double -> Action Double
setRPM = SetRPM

data Action :: * -> * where
  GetTime  :: Action Double
  Gears    :: Action (Int, Int)
  Go       :: Double -> Action (Double, Double)
  Shift    :: Ring -> Direction -> Action ()
  GetRPM   :: Action Double
  SetRPM   :: Double -> Action Double
  Return   :: a -> Action a
  Bind     :: Action a -> (a -> Action b) -> Action b

instance Monad Action where
  return = Return
  (>>=)  = Bind

instance Applicative Action where
  pure = return
  (<*>) = ap

instance Functor Action where
  fmap = liftM

run :: Action a -> Ride a
run GetTime = gets time
run Gears = do
  bg <- gets bgGear
  sm <- gets smGear
  return (bg, sm)
run (Go dist) = do
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
run (Shift r d) = case (r, d) of
  (Big, Up)     -> bgRingUp
  (Big, Down)   -> bgRingDn
  (Small, Up)   -> smRingUp
  (Small, Down) -> smRingDn
run GetRPM = gets rpm
run (SetRPM x) = do
  tell ["Pedal: Change cadence to " ++ printf "%.2f" x]
  modify (\s -> s {rpm = x})
  return x
run (Return x)  = return x
run (Bind m@(Go x) f) =
  case f (0,0) of
    Bind (Go y) k -> run (Bind (Go (x+y)) k)
    _    -> run m >>= \z -> run (f z)
run (Bind m f) = do 
  x <- run m 
  run (f x)
