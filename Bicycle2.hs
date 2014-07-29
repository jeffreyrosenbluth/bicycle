{-# LANGUAGE GADTs                       #-}
{-# LANGUAGE KindSignatures              #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Core
import Control.Applicative
import Control.Lens      hiding (Action)
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
run (Shift r d) = case (r, d) of
  (Big, Up)     -> bgRingUp
  (Big, Down)   -> bgRingDn
  (Small, Up)   -> smRingUp
  (Small, Down) -> smRingDn
run (Cadence x) = do
  tell ["Pedal: Change cadence to " ++ printf "%.2f" x]
  rpm .= x
  return x
run (Return x)  = return x
run (Bind m@(Go x) f) =
  case f (0,0) of
    Bind (Go y) k -> run (Bind (Go (x+y)) k)
    _    -> run m >>= \z -> run (f z)
run (Bind m f) = do 
  x <- run m 
  run (f x)

bikeTrip :: Action () 
bikeTrip =  do
  go 1.5
  shift Big Up
  shift Big Up
  shift Small Down
  shift Small Down
  cadence 100
  go 20
  go 10
  go 3
  shift Big Down
  shift Small Up
  go 5
  return ()
 
main :: IO ()
main = do
  let (s, w) = execRWS (run bikeTrip) bike startTrip 
  putStrLn ""
  putStrLn "------- Bike Trip ---------------------------------"
  putStrLn ""
  mapM_ putStrLn w
  putStrLn ""
  putStrLn ("Distance : " ++ printf "%.2f" (s^.distance) ++ " miles")
  putStrLn ("Time     : " ++ printf "%.2f" (s^.time) ++ " minutes")
  putStrLn ("Avg Speed: " ++ printf "%.2f" (60 * s^.distance / s^.time) ++ " mph") 
