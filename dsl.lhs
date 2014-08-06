Shallow Embedding
=================

Introduction
------------
This is the first in a series of posts discussing embedded domain
specific languages in haskell. The plan is to create a toy DSL and use
it as a running example as we explore shallow and deep embeddings. Along
the way we will meet both the *operational monad* and the *free monad*.
This series is meant to introduce and compare some of the options available
when creating a DSL. EDSLs are an active research topic in computer science,
for a glimpse at some of the deeper issues, there is no better place to start
than Andy Gill\'s [Looking at embedded DSLs](http://delivery.acm.org/10.1145/2620000/2617811/p30-gill.pdf?ip=96.232.159.39&id=2617811&acc=OPEN&key=4D4702B0C3E38B35%2E4D4702B0C3E38B35%2E4D4702B0C3E38B35%2E6D218144511F3437&CFID=524826409&CFTOKEN=31598781&__acm__=1407278022_fe7200df77e4fc00dbd16efb62650ad3).

A domain specific lanuage (DSL) is a
language designed for a specific problem domain. Examples include HTML,
*postscript*, SQL, *diagrams*, *blank-canvas*, and QuickCheck. There are two
flavors of DSLs, the first is a stand alone language with its own interpreter
or compiler. The second is embedded in a host language like haskell. Of the
examples mentioned above, HTML, *postscript*, and SQL are stand alone DSLs, while
*diagrams*, *blank-canvas*, and QuickCheck are embedded. A good introduction to EDSLs
in haskell is Adres Loh\'s talk,
[Haskell for Embedded Domain Specific Languages](https://skillsmatter.com/skillscasts/2872-haskell-for-embedded-domain-specific-languages).

This series is
concerned with EDSLs and the two primary ways to embed them in haskell: shallow
and deep. In a shallow embedding, operations in the DSL are 
just haskell functions operating on haskell values. In a deep embedding an
abstract syntax tree is created and then evaluated. Of course in the real world
we have the complete spectrum from shallow to deep and most DSLs, although closer
to on end thanthe other, are not strictly
shallow or deep.  Rather than go into detail
about the differences between shallow and deep embeddings here we will introduce
our toy DSL and illustrate the differences using it as an example.

In this post we will cover the
example domain, a bicycle ride, and create a shallow EDSL for it. Future
posts will cover deep embeddings.

The bicycle ride language
-------------------------

Our language will model the things that a cyclist can do during a ride: go
a specified distance at the current gear and cadence (rpm), shift gears, and
change cadence. The DSL will keep a running log of all of the cyclist\'s 
actions as well as the elapsed time and average speed of the ride.

We start by modeling a bicycle and the state of a ride which we call `Trip`.
These types and functions are common to all of the DSL versions we will write
and can be found at: 
[Core](https://github.com/jeffreyrosenbluth/bicycle/blob/master/Core.hs).

~~~~ { .haskell }
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
~~~~

The number of teeth on a front chain ring or rear sprocket is an increasing
list of `Int`s represented by the type `Gears`. For our purposes, 
all we need to know about the
bicycle are its available gears and the wheel diameter. The data type 
`Trip` models the current state of the ride including, front gear, back gear,
cadence (rpm), elapsed distance and average time. We can
calculate the speed the bicycle is going from the cadence (rotations per
minute) . In fact 

$$speed = rpm \cdot \pi \cdot \frac{A \cdot B}{C}$$

where speed is measured in inches per minute.

![Gear Ratio](bike-gear-ratio.png)

We will need read only access to the bicycle, the ability to modify the
`Trip` and we want to keep a log of the riders actions.
The RWS (read, write, state) monad fits the bill, where the reader environment
is a `Bicycle`, the writer part is a log (`[String]`) of the riders 
actions and the state is a `Trip`.

~~~~ { .haskell }
type Log       = [String]
data Ring      = Big | Small deriving Show
data Direction = Up | Down deriving Show
type Ride a    = RWS Bicycle Log Trip a
~~~~

The `Ring` and `Direction` types are used in the DSL to specify shifting the
front or back deraillleur either up or down. A program in our DSL will
look something like this,

~~~~ { .haskell }
bikeRide = Ride ()
bikeride = do
 shift Small Down -- to a lower number of teeth
 setRPM 90        -- set the cadence
 go 10            -- go 10 miles at the current gear and cadence.
~~~~

The function returns an object of type `Ride ()` which we can evaluate with
`execRWS`. Notice that our program looks like a sequence of statements. 
Our DSL uses do notation to mimick an imperative language. In this case
the bind function from the RWS monad is used to sequece the actions.

Before moving on to the DSL proper we need a few utility functions
that will be used by all of the DSL versions that we write. 
These functions are in 
[Core](https://github.com/jeffreyrosenbluth/bicycle/blob/master/Core.hs).

~~~~ { .haskell }
nextGear :: Gears -> Int -> Int
nextGear gs g = case elemIndex g gs of
  Nothing -> error "Invalid Gear"
  Just i  -> if i == length gs - 1
             then last gs
             else gs !! (i+1)
~~~~

Given a a list `Gears` and the current gear (number of teeth), return the
new gear. If we are already at the top gear, just return it. Since this is
an interanl function that will not be exposed by our DSL, we can guarantee
that the error branch is never executed.

`prevGear is defined analgously to `nextGear` refer to the source file for 
the implementation.

~~~~ { .haskell }
prevGear :: Gears -> Int -> Int
~~~~

Calculate the speed where diameter is in inches, distance in miles and the
result is miles per second.

~~~~ { .haskell }
speed :: Int -> Int -> Double -> Double -> Double
speed bg sm diam r = 
  fromIntegral bg / fromIntegral sm * diam * r * pi / (60 * 12 * 5280)
~~~~

The following four functions are used for shifting gears and are all defined in a
manner similar to `bgRingUp`. This function
demonstrates how the components of the RWS monad are utilized. We get the
current gear of the big ring with `gets`, the bicycle with `ask` and we
log the action with `tell`. Finally, we modify the state with `modify`.

~~~~ { .haskell }
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
smRingUp :: Ride ()
smRingDn :: Ride ()
~~~~

Our first DSL, a shallow embedding, is comprised of 6 haskell functions. See
[Bicycle1](https://github.com/jeffreyrosenbluth/bicycle/blob/master/Bicycle1.hs)
for the full source code.

- `getTime` - returns the total time for the ride so far.
- `gears` - returns the pair (big gear, small gear).
- `go` - ride for specified miles at current cadence and gear
   and return the speed and time for the leg.
- `shift` -  big or small ring, up or down.
- `getRPM` - return the cadence.
- `setRPM` - set the cadence.

The implementation of these functions is straight forward. Notice that they
all return a value wrapped in the `Ride` monad. This allows use to chain
instructions using bind and for future actions to depend on previous results.

~~~~ { .haskell }
getTime :: Ride Double
getTime = gets time

gears :: Ride (Int, Int)
gears = do
  bg <- gets bgGear
  sm <- gets smGear
  return (bg, sm)

go :: Double -> Ride (Double, Double)
go dist = do
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

shift :: Ring -> Direction -> Ride ()
shift r d = case (r, d) of
  (Big, Up)     -> bgRingUp
  (Big, Down)   -> bgRingDn
  (Small, Up)   -> smRingUp
  (Small, Down) -> smRingDn

getRPM :: Ride Double
getRPM = gets rpm

setRPM :: Double -> Ride ()
setRPM x = do
  tell ["Pedal: Change setRPM to " ++ printf "%.2f" x]
  modify (\s -> s {rpm = x})
~~~~

We are finally ready to write a program in the bicycle ride DSL.
First we setup our bike.

~~~~ { .haskell }
bgRing, smRing :: Gears
bgRing = [36, 52]
smRing = [11, 12, 13, 14, 15, 16, 17, 19, 21, 23, 25]

diameter :: Double
diameter = 26.3 -- inches

bike :: Bicycle
bike = Bicycle bgRing smRing diameter
~~~~

The nice part about embedded DSLs is that you can use the constructs of the
host language as needed. Here we use haskell to extend our language with two
new functions, one to shfit the big ring to its largest gear and the other to
shift the small ring to its highest gear (fewest teeth).

See the full source code at:
[Trip](https://github.com/jeffreyrosenbluth/bicycle/blob/master/Trip.hs).

~~~~ { .haskell }
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
~~~~

Our examaple program takes a argument mph, if after the 20 mile segment of
the ride, the speed exceeds this agrument then the back derailleur is shfited
down. 

~~~~ {.haskel }
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
  topBig   -- shift chain ring all the way up
  topSmall -- shift the back gear to its fastest
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
~~~~

Given a `Bicycle` and an initial `Trip`, excute the ride and display the
results. Here `eval` is a synonym for `execRWS`.

~~~~ { .haskell }
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
~~~~

Upon building and running the programs Trip we see the following output

    ---------------- Bike Trip --------------------

    Going: 1.50 miles at 12.07 mph in 7.46 minutes.
    Shift: Small ring down to 19.
    Shift: Small ring down to 17.
    Pedal: Change setRPM to 100.00
    Going: 20.00 miles at 16.57 mph in 72.42 minutes.
    Shift: Small ring down to 16.
    Going: 10.00 miles at 17.60 mph in 34.08 minutes.
    Going: 3.00 miles at 17.60 mph in 10.22 minutes.
    Shift: Small ring up to 17.
    Shift: Large ring up to 52.
    Shift: *** Already at biggest gear.
    Shift: Small ring down to 16.
    Shift: Small ring down to 15.
    Shift: Small ring down to 14.
    Shift: Small ring down to 13.
    Shift: Small ring down to 12.
    Shift: Small ring down to 11.
    Shift: *** Already at highest gear.
    Going: 5.00 miles at 36.99 mph in 8.11 minutes.
    Shift: Large ring down to 36.
    Shift: Small ring up to 12.
    Shift: Small ring up to 13.
    Going: 2.50 miles at 21.67 mph in 6.92 minutes.

    Distance : 42.00 miles
    Time     : 139.22 minutes
    Avg Speed: 18.10 mph
    -----------------------------------------------
