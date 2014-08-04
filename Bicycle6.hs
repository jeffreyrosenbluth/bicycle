{-# LANGUAGE GADTs         #-}

module Bicycle6

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
import Control.Monad.Operational
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
  
data ActionF r where
  GetTime :: ActionF Double
  Gears :: ActionF (Int, Int)
  Go :: Double -> ActionF (Double, Double)
  Shift :: Ring -> Direction -> ActionF ()
  GetRPM :: ActionF Double
  SetRPM :: Double -> ActionF Double

type Action = Program ActionF

run :: Action a -> Ride a
run m = case view m of
  Return x -> return x
  GetTime :>>= k -> do
    tm <-gets time
    run (k tm)
  Gears :>>= k -> do
    bg <- gets bgGear
    sm <- gets smGear
    run (k (bg, sm))
  Go dist :>>= k -> 
    case view $ k (0,0) of
      Go dist' :>>= k' ->
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
  Shift r d :>>= k -> do
    case (r, d) of
      (Big, Up)     -> bgRingUp
      (Big, Down)   -> bgRingDn
      (Small, Up)   -> smRingUp
      (Small, Down) -> smRingDn
    run (k ())
  GetRPM :>>= k -> do
    c <- gets rpm
    run (k c)
  SetRPM x :>>= k -> do
    tell ["Pedal: Change cadence to " ++ printf "%.2f" x]
    modify (\s -> s {rpm = x})
    run (k x)
