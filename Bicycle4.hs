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
go dist = Wrap $ Go dist Return

shift :: Ring -> Direction -> Action Int
shift r d = Wrap $ Shift r d Return

cadence :: Double -> Action Double
cadence x = Wrap $ Cadence x Return
  
data Free :: (* -> *) -> * -> * where
  Return :: a -> Free f a
  Wrap   :: f (Free f a) -> Free f a

instance Functor f => Monad (Free f) where
  return = Return

  Return x >>= f = f x
  Wrap m   >>= f = Wrap $ fmap (>>= f) m

instance Functor f => Applicative (Free f) where
  pure = return
  (<*>) = ap

instance Functor f => Functor (Free f) where
  fmap = liftM

data ActionF :: * -> * where
  Go       :: Double -> ((Double, Double) -> r) -> ActionF r
  Shift    :: Ring -> Direction -> (Int -> r) -> ActionF r
  Cadence  :: Double -> (Double -> r) -> ActionF r

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
run (Wrap (Shift r d f)) = do
  n <- case (r, d) of
        (Big, Up)     -> bgRingUp
        (Big, Down)   -> bgRingDn
        (Small, Up)   -> smRingUp
        (Small, Down) -> smRingDn
  run (f n)
run (Wrap (Cadence x f)) = do
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
  display s w
