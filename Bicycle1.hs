{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Bicycle1

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
  ) where

import Core
import Control.Monad.RWS
import Text.Printf

getTime :: Ride Double
getTime = gets time

gears :: Ride (Int, Int)
gears = do
  bg <- gets bgGear
  sm <- gets smGear
  return (bg, sm)

go :: Double -> Ride (Double, Double)
go dist = do
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

shift :: Ring -> Direction -> Ride ()
shift r d = case (r, d) of
  (Big, Up)     -> bgRingUp
  (Big, Down)   -> bgRingDn
  (Small, Up)   -> smRingUp
  (Small, Down) -> smRingDn

getRPM :: Ride Double
getRPM = gets rpm

setRPM :: Double -> Ride ()
setRPM x = do
  tell ["Pedal: Change setRPM to " ++ printf "%.2f" x]
  modify (\s -> s {rpm = x})
