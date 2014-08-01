{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Bicycle5
import Text.Printf
import Control.Monad (unless)

bgRing, smRing :: Gears
bgRing = [36, 52]
smRing = [11, 12, 13, 14, 15, 16, 17, 19, 21, 23, 25]

-- Inches.
diameter :: Double
diameter = 26.3

-- This bike has 22 gears.
bike :: Bicycle
bike = Bicycle bgRing smRing diameter

topBig = do
  (bg, _) <- gears
  shift Big Up
  (bg', _) <- gears
  unless (bg == bg') topBig

topSmall = do
 (_, sm) <- gears
 shift Small Down
 (_, sm') <- gears
 unless (sm == sm') topSmall

bikeRide mph =  do
  go 1.5
  shift Small Down
  shift Small Down
  setRPM 100
  (s, _) <- go 20
  shift Small (if (s * 3600) > mph then Up else Down)
  go 10
  go 3
  shift Small Up
  topBig
  topSmall
  go 5
  tm <- getTime
  if tm > 90
    then do
      shift Big Down
      shift Small Up
      shift Small Up
    else return ()
  go 2.5
  return ()

main :: IO ()
main = do
  let (s, w) = eval (run $ bikeRide 20) bike (Trip 90 36 21 0 0)
  putStrLn ""
  putStrLn "---------------- Bike Trip --------------------\n"
  mapM_ putStrLn w
  putStrLn ""
  putStrLn ("Distance : " ++ printf "%.2f" (distance s) ++ " miles")
  putStrLn ("Time     : " ++ printf "%.2f" (time s) ++ " minutes")
  putStrLn ("Avg Speed: " ++ printf "%.2f" (60 * distance s / time s) ++ " mph") 
  putStrLn "-----------------------------------------------\n"
