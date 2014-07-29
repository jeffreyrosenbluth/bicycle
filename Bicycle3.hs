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
go dist = Go dist Return

shift :: Ring -> Direction -> Action Int
shift r d = Shift r d Return

cadence :: Double -> Action Double
cadence x = Cadence x Return
  
-- No longer a proper GADT, equivalent to
-- data Action a = BgRingUp (Int -> Action a) | ...
data Action :: * -> * where
  Go       :: Double -> ((Double, Double) -> Action a) -> Action a
  Shift    :: Ring -> Direction -> (Int -> Action a) -> Action a
  Cadence  :: Double -> (Double -> Action a) -> Action a
  Return   :: a -> Action a

instance Monad Action where
  return = Return
  -- Bind --
  Go x f      >>= k = Go x ((>>= k) . f)
  Shift r d f >>= k = Shift r d ((>>= k) . f)
  Cadence x f >>= k = Cadence x ((>>= k) . f)
  Return   x  >>= k = k x

instance Applicative Action where
  pure = return
  (<*>) = ap

instance Functor Action where
  fmap = liftM

run :: Action a -> Ride a
run (Go dist f) = 
  case f (0,0) of
    Go dist' g -> run (Go (dist + dist') g)
    _          -> do
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
      run (f (sp, tm))
run (Shift r d f) = do
  n <- case (r, d) of
        (Big, Up)     -> bgRingUp
        (Big, Down)   -> bgRingDn
        (Small, Up)   -> smRingUp
        (Small, Down) -> smRingDn
  run (f n)
run (Cadence x f) = do
  tell ["Pedal: Change cadence to " ++ printf "%.2f" x]
  rpm .= x
  run (f x)
run (Return x) = return x

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
