{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Bicycle3

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
getTime = GetTime Return

gears :: Action (Int, Int)
gears = Gears Return

go :: Double -> Action (Double, Double)
go dist = Go dist Return

shift :: Ring -> Direction -> Action ()
shift r d = Shift r d Return

setRPM :: Double -> Action Double
setRPM x = SetRPM x Return

getRPM :: Action Double
getRPM = GetRPM Return
  
-- data Action :: * -> * where
  -- Go       :: Double -> ((Double, Double) -> Action a) -> Action a
  -- Shift    :: Ring -> Direction -> (Int -> Action a) -> Action a
  -- Cadence  :: Double -> (Double -> Action a) -> Action a
  -- Return   :: a -> Action a

data Action a
  = GetTime (Double -> Action a)
  | Gears ((Int, Int) -> Action a)
  | Go Double ((Double, Double) -> Action a)
  | Shift Ring Direction (() -> Action a)
  | SetRPM Double (Double -> Action a)
  | GetRPM (Double -> Action a)
  | Return a

instance Monad Action where
  return = Return
  -- Bind --
  GetTime f   >>= k = GetTime ((>>= k) . f)
  Gears f     >>= k = Gears ((>>= k) . f)
  Go x f      >>= k = Go x ((>>= k) . f)
  Shift r d f >>= k = Shift r d ((>>= k) . f)
  SetRPM x f  >>= k = SetRPM x ((>>= k) . f)
  GetRPM f    >>= k = GetRPM ((>>= k) . f)
  Return x    >>= k = k x

instance Applicative Action where
  pure = return
  (<*>) = ap

instance Functor Action where
  fmap = liftM

run :: Action a -> Ride a
run (GetTime f) = do
  tm <- gets time
  run (f tm)
run (Gears f) = do
  bg <- gets bgGear
  sm <- gets smGear
  run (f (bg, sm))
run (Go dist f) = 
  case f (0,0) of
    Go dist' g -> run (Go (dist + dist') g)
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
run (Shift r d f) = do
  n <- case (r, d) of
        (Big, Up)     -> bgRingUp
        (Big, Down)   -> bgRingDn
        (Small, Up)   -> smRingUp
        (Small, Down) -> smRingDn
  run (f n)
run (GetRPM f) = do
  c <- gets rpm
  run (f c)
run (SetRPM x f) = do
  tell ["Pedal: Change cadence to " ++ printf "%.2f" x]
  modify (\s -> s {rpm = x})
  run (f x)
run (Return x) = return x
