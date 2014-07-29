{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Core
import Control.Lens      ((.=), (+=), (^.), use)
import Control.Monad.RWS
import Text.Printf

go :: Double -> Ride (Double, Double)
go dist = do
  bg <- use bgGear
  sm <- use smGear
  r  <- use rpm
  b  <- ask
  let sp = speed bg sm (b^.wheelDiam) r
      tm = dist / sp / 60
  tell ["Going: " ++ printf "%.2f" dist ++ " miles at " 
                  ++ printf "%.2f" (sp * 3600) ++ " mph in " 
                  ++ printf "%.2f" tm ++ " minutes."]
  time += tm
  distance += dist
  return (sp, tm)

shift :: Ring -> Direction -> Ride Int
shift r d = case (r, d) of
  (Big, Up)     -> bgRingUp
  (Big, Down)   -> bgRingDn
  (Small, Up)   -> smRingUp
  (Small, Down) -> smRingDn

cadence :: Double -> Ride Double
cadence x = do
  tell ["Pedal: Change cadence to " ++ printf "%.2f" x]
  rpm .= x
  return x
  
bikeTrip :: Double -> Ride ()
bikeTrip mph =  do
  go 1.5
  shift Big Up
  shift Big Up
  shift Small Down
  shift Small Down
  cadence 100
  (s, _) <- go 20
  shift Small (if (s * 3600) > mph then Up else Down)
  go 10
  go 3
  shift Big Down
  shift Small Up
  go 5
  return ()

main :: IO ()
main = do
  let (s, w) = execRWS (bikeTrip 20) bike startTrip 
  display s w
