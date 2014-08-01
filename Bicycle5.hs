{-# LANGUAGE DeriveFunctor #-}

module Bicycle5

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
import Control.Monad.Free        (Free (..), liftF)
import Control.Monad.Free.Church (F, fromF)
import Control.Monad.RWS
import Text.Printf

getTime :: Action Double
getTime = liftF $ GetTime id

gears :: Action (Int, Int)
gears = liftF $ Gears id

go :: Double -> Action (Double, Double)
go dist = liftF $ Go dist id

shift :: Ring -> Direction -> Action ()
shift r d = liftF $ Shift r d ()

getRPM :: Action Double
getRPM = liftF $ GetRPM id

setRPM :: Double -> Action Double
setRPM x = liftF $ SetRPM x id
  
data ActionF r
  = GetTime (Double -> r)
  | Gears ((Int, Int) -> r)
  | Go Double ((Double, Double) -> r)
  | Shift Ring Direction r
  | GetRPM (Double -> r)
  | SetRPM Double (Double -> r)
    deriving Functor

type Action = F ActionF

run :: Action a -> Ride a
run = runF . fromF

runF :: Free ActionF a -> Ride a
runF (Free (GetTime f)) = do
  tm <- gets time
  runF (f tm)
runF (Free (Gears f)) = do
  bg <- gets bgGear
  sm <- gets smGear
  runF (f (bg, sm))
runF (Free (Go dist f)) = 
  case f (0,0) of
    (Free (Go dist' g)) -> runF (Free (Go (dist + dist') g))
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
      runF (f (sp, tm))
runF (Free (Shift r d x)) = do
  case (r, d) of
    (Big, Up)     -> bgRingUp
    (Big, Down)   -> bgRingDn
    (Small, Up)   -> smRingUp
    (Small, Down) -> smRingDn
  runF x
runF (Free (GetRPM f)) = do
  c <- gets rpm
  runF (f c)
runF (Free (SetRPM x f)) = do
  tell ["Pedal: Change cadence to " ++ printf "%.2f" x]
  modify (\s -> s {rpm = x})
  runF (f x)
runF (Pure x) = return x
