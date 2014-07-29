{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad.RWS
import Data.List         (elemIndex)
import Data.Maybe        (maybe)
import Text.Printf

type Gears = [Int]

data Bicycle = Bicycle 
  { _chainRings :: Gears
  , _sprockets  :: Gears
  , _wheelDiam  :: Double
  } deriving Show

makeLenses ''Bicycle

data Trip = Trip
  { _rpm      :: Double
  , _bgGear   :: Int
  , _smGear   :: Int
  , _distance :: Double
  , _time     :: Double
  } deriving Show

makeLenses ''Trip

type Ride a = RWS Bicycle [String] Trip a

nextGear :: Gears -> Int -> Int
nextGear gs g = case elemIndex g gs of
  Nothing -> error "Invalid Gear"
  Just i  -> if i == length gs - 1
             then last gs
             else gs !! (i+1)

prevGear :: Gears -> Int -> Int
prevGear gs g = case elemIndex g gs of
  Nothing -> error "Invalid Gear"
  Just i  -> if i == 0
             then head gs
             else gs !! (i-1)

-- Diameter is in inches, Distance in miles.
speed :: Int -> Int -> Double -> Double -> Double
speed bg sm diam rpm = 
  fromIntegral bg / fromIntegral sm * diam * rpm  * pi / (60 * 12 * 5280)

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

data Ring = Big | Small deriving Show

data Direction = Up | Down deriving Show

shift :: Ring -> Direction -> Ride Int
shift r d = case (r, d) of
  (Big, Up)     -> bgRingUp
  (Big, Down)   -> bgRingDn
  (Small, Up)   -> smRingUp
  (Small, Down) -> smRingDn

bgRingUp :: Ride Int
bgRingUp = do
  bg <- use bgGear
  b  <- ask
  let cr = b^.chainRings
      r  = nextGear cr bg
  if bg == r
    then tell ["Shift: *** Already at biggest gear."]
    else tell ["Shift: Large ring up to " ++ show r ++ "."]
  bgGear .= r
  return r

bgRingDn :: Ride Int
bgRingDn = do
  bg <- use bgGear
  b <- ask
  let cr = b^.chainRings
      r  = prevGear cr bg
  if bg == r
    then tell ["Shift: *** Already at smallest gear."]
    else tell ["Shift: Large ring down to " ++ show r ++ "."]
  bgGear .= r
  return r

smRingUp :: Ride Int
smRingUp = do
  sm <- use smGear
  b  <- ask
  let cr = b^.sprockets
      r  = nextGear cr sm
  if sm == r
    then tell ["Shift: *** Already at lowest gear."]
    else tell ["Shift: Small ring up to " ++ show r]
  smGear .= r
  return r

smRingDn :: Ride Int
smRingDn = do
  sm <- use smGear
  b  <- ask
  let cr = b^.sprockets
      r  = prevGear cr sm

  if sm == r
    then tell ["Shift: *** Already at highest gear."]
    else tell ["Shift: Small ring down to " ++ show r]
  smGear .= r
  return r

cadence :: Double -> Ride Double
cadence x = do
  tell ["Pedal: Change cadence to " ++ printf "%.2f" x]
  rpm .= x
  return x
  
-------------------------------------------------------------------------------

bgRing = [36, 52]
smRing = [11, 12, 13, 14, 15, 16, 17, 19, 21, 23, 25]
diameter = 26.3 -- inches
bike = Bicycle bgRing smRing diameter
startTrip = Trip 90 36 21 0 0

bikeTrip :: Ride ()
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
 
main = do
  let (s, w) = execRWS bikeTrip bike startTrip 
  putStrLn ""
  putStrLn "------- Bike Trip ---------------------------------"
  putStrLn ""
  mapM_ putStrLn w
  putStrLn ""
  putStrLn ("Distance : " ++ printf "%.2f" (s^.distance) ++ " miles")
  putStrLn ("Time     : " ++ printf "%.2f" (s^.time) ++ " minutes")
  putStrLn ("Avg Speed: " ++ printf "%.2f" (60 * s^.distance / s^.time) ++ " mph") 
  putStrLn ""
