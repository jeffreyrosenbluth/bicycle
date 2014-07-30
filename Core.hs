module Core where

import Control.Monad.RWS
import Data.List         (elemIndex)

type Gears = [Int]

data Bicycle = Bicycle 
  { chainRings :: Gears
  , sprockets  :: Gears
  , wheelDiam  :: Double
  } deriving Show

data Trip = Trip
  { rpm      :: Double
  , bgGear   :: Int
  , smGear   :: Int
  , distance :: Double
  , time     :: Double
  } deriving Show

type Log = [String]

data Ring = Big | Small deriving Show

data Direction = Up | Down deriving Show

type Ride a = RWS Bicycle Log Trip a

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
-- Result is miles per second
speed :: Int -> Int -> Double -> Double -> Double
speed bg sm diam r = 
  fromIntegral bg / fromIntegral sm * diam * r * pi / (60 * 12 * 5280)

bgRingUp :: Ride ()
bgRingUp = do
  bg  <- gets bgGear
  b   <- ask
  let cr = chainRings b
      r  = nextGear cr bg
  tell $ if bg == r
    then ["Shift: *** Already at biggest gear."]
    else ["Shift: Large ring up to " ++ show r ++ "."]
  modify (\s -> s {bgGear = r})

bgRingDn :: Ride ()
bgRingDn = do
  bg <- gets bgGear
  b  <- ask
  let cr = chainRings b
      r  = prevGear cr bg
  tell $ if bg == r
    then ["Shift: *** Already at smallest gear."]
    else ["Shift: Large ring down to " ++ show r ++ "."]
  modify (\s -> s {bgGear = r})

smRingUp :: Ride ()
smRingUp = do
  sm <- gets smGear
  b  <- ask
  let cr = sprockets b
      r  = nextGear cr sm
  tell $ if sm == r
    then ["Shift: *** Already at lowest gear."]
    else ["Shift: Small ring up to " ++ show r ++ "."]
  modify (\s -> s {smGear = r})

smRingDn :: Ride ()
smRingDn = do
  sm <- gets smGear
  b  <- ask
  let cr = sprockets b
      r  = prevGear cr sm
  tell $ if sm == r
    then ["Shift: *** Already at highest gear."]
    else ["Shift: Small ring down to " ++ show r ++ "."]
  modify (\s -> s {smGear = r})

eval :: Ride () -> Bicycle -> Trip -> (Trip, Log)
eval = execRWS
