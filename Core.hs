{-# LANGUAGE TemplateHaskell #-}

module Core where

import Control.Lens
import Control.Monad.RWS
import Data.List         (elemIndex)

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

bgRing, smRing :: [Int]
bgRing = [36, 52]
smRing = [11, 12, 13, 14, 15, 16, 17, 19, 21, 23, 25]

-- In inches.
diameter :: Double
diameter = 26.3

-- This bike has 22 gears.
bike :: Bicycle
bike = Bicycle bgRing smRing diameter

-- We start at 90 rpm big ring 36, small ring 21.
-- Distance and time start at 0.
startTrip :: Trip
startTrip = Trip 90 36 21 0 0

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
speed bg sm diam r = 
  fromIntegral bg / fromIntegral sm * diam * r  * pi / (60 * 12 * 5280)

data Ring = Big | Small deriving Show

data Direction = Up | Down deriving Show

bgRingUp :: Ride Int
bgRingUp = do
  bg <- use bgGear
  b  <- ask
  let cr = b^.chainRings
      r  = nextGear cr bg
  tell $ if bg == r
    then ["Shift: *** Already at biggest gear."]
    else ["Shift: Large ring up to " ++ show r ++ "."]
  bgGear .= r
  return r

bgRingDn :: Ride Int
bgRingDn = do
  bg <- use bgGear
  b <- ask
  let cr = b^.chainRings
      r  = prevGear cr bg
  tell $ if bg == r
    then ["Shift: *** Already at smallest gear."]
    else ["Shift: Large ring down to " ++ show r ++ "."]
  bgGear .= r
  return r

smRingUp :: Ride Int
smRingUp = do
  sm <- use smGear
  b  <- ask
  let cr = b^.sprockets
      r  = nextGear cr sm
  tell $ if sm == r
    then ["Shift: *** Already at lowest gear."]
    else ["Shift: Small ring up to " ++ show r]
  smGear .= r
  return r

smRingDn :: Ride Int
smRingDn = do
  sm <- use smGear
  b  <- ask
  let cr = b^.sprockets
      r  = prevGear cr sm
  tell $ if sm == r
    then ["Shift: *** Already at highest gear."]
    else ["Shift: Small ring down to " ++ show r]
  smGear .= r
  return r
