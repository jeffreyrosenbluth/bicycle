{-# LANGUAGE GADTs                       #-}
{-# LANGUAGE KindSignatures              #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Core
import Control.Applicative
import Control.Monad.RWS
import Text.Printf

go :: Double -> Action (Double, Double)
go = Go 

shift :: Ring -> Direction -> Action Int
shift = Shift

cadence :: Double -> Action Double
cadence = Cadence

data Action :: * -> * where
  Go       :: Double -> Action (Double, Double)
  Shift    :: Ring -> Direction -> Action Int
  Cadence  :: Double -> Action Double
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
run (Cadence x) = do
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

bikeTrip :: Double -> Action () 
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
  let (s, w) = execRWS (run $ bikeTrip 20) bike startTrip 
  display s w
