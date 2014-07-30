{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Core
import Control.Applicative
import Control.Monad.RWS
import Text.Printf

go :: Double -> Action (Double, Double)
go dist = Wrap $ Go dist Return

shift :: Ring -> Direction -> Action Int
shift r d = Wrap $ Shift r d Return

cadence :: Double -> Action Double
cadence x = Wrap $ Cadence x Return
  
data Free f a
  = Return a 
  | Wrap (f (Free f a))

instance Functor f => Monad (Free f) where
  return = Return

  Return x >>= f = f x
  Wrap m   >>= f = Wrap $ fmap (>>= f) m

instance Functor f => Applicative (Free f) where
  pure = return
  (<*>) = ap

instance Functor f => Functor (Free f) where
  fmap = liftM

data ActionF r
  = Go Double ((Double, Double) -> r)
  | Shift Ring Direction (Int -> r)
  | Cadence Double (Double -> r)

instance Functor ActionF where
  fmap f (Go x g)      = Go x (f . g)
  fmap f (Shift r d g) = Shift r d (f . g)
  fmap f (Cadence x g) = Cadence x (f . g)

type Action = Free ActionF

run :: Action a -> Ride a
run (Wrap (Go dist f)) = 
  case f (0,0) of
    (Wrap (Go dist' g)) -> run (Wrap (Go (dist + dist') g))
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
run (Wrap (Shift r d f)) = do
  n <- case (r, d) of
        (Big, Up)     -> bgRingUp
        (Big, Down)   -> bgRingDn
        (Small, Up)   -> smRingUp
        (Small, Down) -> smRingDn
  run (f n)
run (Wrap (Cadence x f)) = do
  tell ["Pedal: Change cadence to " ++ printf "%.2f" x]
  modify (\s -> s {rpm = x})
  run (f x)
run (Return x) = return x

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
