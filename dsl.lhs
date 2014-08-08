Shallow Embedding
=================

Introduction
------------
This is the first in a series of posts discussing embedded domain
specific languages in haskell. The plan is to create a toy DSL that will be used
as a running example as we explore shallow and deep embeddings. Along
the way we will meet both the *operational monad* and the *free monad*.

EDSLs are an active research topic in computer science,
for a glimpse at some of the deeper issues, a good place to start
is Andy Gill\'s [Looking at embedded DSLs](http://delivery.acm.org/10.1145/2620000/2617811/p30-gill.pdf?ip=96.232.159.39&id=2617811&acc=OPEN&key=4D4702B0C3E38B35%2E4D4702B0C3E38B35%2E4D4702B0C3E38B35%2E6D218144511F3437&CFID=524826409&CFTOKEN=31598781&__acm__=1407278022_fe7200df77e4fc00dbd16efb62650ad3).

A domain specific language (DSL) is a
language designed for a specific problem domain. Examples include *HTML*,
*postscript*, *SQL*, *diagrams*, *blank-canvas*, and *QuickCheck*. There are two
flavors of DSLs, the first is a stand alone language with its own interpreter
or compiler. The second is embedded in a host language like haskell. Of the
examples mentioned above, *HTML*, *postscript*, and *SQL* are stand alone DSLs, while
*diagrams*, *blank-canvas*, and *QuickCheck* are embedded. A good introduction to EDSLs
in haskell is Andres Loh\'s talk,
[Haskell for Embedded Domain Specific Languages](https://skillsmatter.com/skillscasts/2872-haskell-for-embedded-domain-specific-languages).

There are two basic ways to embed a DSL in haskell: shallow
and deep. In a shallow embedding, operations in the DSL are 
just haskell functions operating on haskell values. In a deep embedding we
first build an abstract syntax tree and then evaluate it using an interpreter. 
Of course in the real world a DSL can be anywhere on a full spectrum
from shallow to deep and most DSLs, although closer
to on end than the other, are not strictly
shallow or deep.  Rather than go into detail
about the differences between shallow and deep embeddings here, we will introduce
our toy DSL and illustrate the differences using it as an example.

In this post we will cover the
example domain, a bicycle ride, and create a shallow EDSL for it. 
Deep embedding will be covered in subsequent posts.

The bicycle ride language -- BRL
--------------------------------

Our language will model a subset of the things that a road cyclist can do on a ride: go
a specified distance, shift gears, and
change cadence (rotations per minute). The DSL will keep a running log of all of the cyclist\'s 
actions as well as the elapsed time and average speed of the ride.

We start by modeling a bicycle and the state of a ride which we call `Trip`.
The following types and functions are common to all of the DSL versions we will write
so we have separated them out in their own module,
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

The type `Gears` represents a single chain ring or sprocket, the only
information we need is its number of teeth. This will allow us to calculate
the gear ratio and hence, in combination with the wheel diameter and cadence
the speed at which the bicycle if moving. 

$$speed = rpm \cdot \pi \cdot \frac{A \cdot B}{C}$$

where speed is measured in inches per minute.

![Gear Ratio](bike-gear-ratio.png)

The data type `Trip` models the current state of the ride including, front gear, back gear,
cadence (rpm), elapsed distance and average time.
We will need read only access to the bicycle type, the ability to modify the
`Trip` and we will want to keep a log of the riders actions.
The RWS (read, write, state) monad fits the bill, where the reader environment
is a `Bicycle`, the writer part is a log (`[String]`) of the riders 
actions and the state is a `Trip`.

~~~~ { .haskell }
type Log       = [String]
data Ring      = Big | Small deriving Show
data Direction = Up  | Down deriving Show
type Ride a    = RWS Bicycle Log Trip a
~~~~

The `Ring` and `Direction` types are used to help make BRL read more like
english. A program in our DSL will look something like this,

~~~~ { .haskell }
bikeRide = Ride ()
bikeRide = do
 shift Small Down -- to a lower number of teeth
 setRPM 90        -- set the cadence
 go 10            -- go 10 miles at the current gear and cadence.
~~~~

The function returns an object of type `Ride ()` which we can evaluate with
`execRWS`. Notice that our program looks like a sequence of statements. 
Our DSL uses do notation to mimic an imperative language. 
The bind instance from the RWS monad sequences the actions.

DSL Functions
-------------
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

prevGear :: Gears -> Int -> Int
...
~~~~

Given a a list `Gears` and the current gear (number of teeth), return the
new gear. If we are already at the top gear, just return it. Since this is
an internal function that will not be exposed by our DSL, we can guarantee
that the error branch is never executed.

`speed` calculates the speed of the bike,  where diameter is in inches, 
distance in miles and the result is miles per second.

~~~~ { .haskell }
speed :: Int -> Int -> Double -> Double -> Double
speed bg sm diam r = 
  fromIntegral bg / fromIntegral sm * diam * r * pi / (60 * 12 * 5280)
~~~~

The following four functions are used for shifting gears and are all 
very similar to the first `bgRingUp`, to save space we only show one. 
The function
demonstrates how the components of the RWS monad are utilized. We get the
current gear of the big ring with `gets`, the `Bicycle` with `ask` and we
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

The Shallow EDSL
----------------
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

Programming in BRL
------------------
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
new functions, one to shift the big ring to its largest gear and the other to
shift the small ring to its highest gear (fewest teeth).

The file
[Trip](https://github.com/jeffreyrosenbluth/bicycle/blob/master/Trip.hs)
contains a these functions, plus a test harness that we will use to display
the results of a BRL program.

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

Our example program takes a argument `mph`, if after the 20 mile segment of
the ride, the speed exceeds this argument then the back derailleur is shifted
down. The rest of the code should be pretty self explanatory -- that\'s one
of the benefits of using haskell as our host language.

~~~~ {.haskell }
bikeRide mph =  do
  go 2.5
  shift Small Down
  setRPM 100
  (s, _) <- go 20
  shift Small (if (s * 3600) > mph then Up else Down)
  go 7.5
  go 7.5
  topBig
  topSmall
  go 5
  tm <- getTime
  if tm > 90
    then do
      shift Big Down
      shift Small Up
    else return ()
  go 2.5
  return ()
~~~~

Given a `Bicycle` and an initial `Trip`, execute the ride and display the
results. Here `eval` is a synonym for `execRWS`.
Upon building and running the program Trip.hs we get the following output


    ---------------- Bike Trip --------------------

    Going: 2.50 miles at 13.34 mph in 11.24 minutes.
    Shift: Small ring down to 17.
    Pedal: Change setRPM to 100.00
    Going: 20.00 miles at 16.57 mph in 72.42 minutes.
    Shift: Small ring down to 16.
    Going: 7.50 miles at 17.60 mph in 25.56 minutes.
    Going: 7.50 miles at 17.60 mph in 25.56 minutes.
    Shift: Large ring up to 52.
    Shift: *** Already at biggest gear.
    Shift: Small ring down to 15.
    Shift: Small ring down to 14.
    Shift: Small ring down to 13.
    Shift: Small ring down to 12.
    Shift: Small ring down to 11.
    Shift: *** Already at highest gear.
    Going: 5.00 miles at 36.99 mph in 8.11 minutes.
    Shift: Large ring down to 36.
    Shift: Small ring up to 12.
    Going: 2.50 miles at 23.47 mph in 6.39 minutes.

    Distance : 45.00 miles
    Time     : 149.29 minutes
    Avg Speed: 18.09 mph
    -----------------------------------------------

Summary
-------
We\'ve done nothing fancy to create our toy EDSL, no GHC
extensions or libraries, just straight up haskell using the RWS monad. In part
2 we will show how GADTs will help us to embed our DSL more deeply. And explore
some of the advantages of a deep embedding.
