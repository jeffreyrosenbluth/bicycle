Part I - Shallow Embedding
=========================

Introduction
------------
This is the first in a series of posts discussing embedded domain
specific languages in haskell. A domain specific lanuage (DSL) is a
language designed for a specific problem domain, as opposed to a genreal
purpose programming language.  Examples include HTML,
postscript, SQL, diagrams, blank-canvas, and Raserific. There are two main
flavors of DSLs, the first is a stand alone language with it's own interpreter
or compiler. The second is embedded in a host language like haskell. Of the
examples mentioned above, HTML, postscript, and SQL are stand alone DSLs, while
diagrams, blank-canvas, and QuickCheck are embedded, EDSLs. This series is
concerned with EDSLs in haskell and the two primary types: shallow embedding
and deep embedding. In a shallow embedding operations in the DSL are typically
just haskell functions. On the other hand in a deeply embedded language an
abstract syntax tree is created and then evaluated. Rather than go into detail
about the differences between shallow and deep embeddings this series of posts
will use a running toy DSL to illustrate. In this post we will cover the
example domain, a bicycle ride and create a shallow EDSL of it. The future
posts will cover deep embeddings, the operational monad and the free monad.

The bicycle ride language
-------------------------

Our language will model the things that a cyclist can do during a ride: go
a specified distance at the current gear and cadence (rpm), shift gears, and
change cadence. The full source code for this section is at,
[Core](https://github.com/jeffreyrosenbluth/bicycle/blob/master/Core.hs),
[Bicycle1](https://github.com/jeffreyrosenbluth/bicycle/blob/master/Bicycle1.hs)
and [Trip](https://github.com/jeffreyrosenbluth/bicycle/blob/master/Trip.hs).

We start by modeling a bicycle.

> type Gears = [Int]
>
> data Bicycle = Bicycle 
>   { chainRings :: Gears
>   , sprockets  :: Gears
>   , wheelDiam  :: Double
>   } deriving Show
>
> data Trip = Trip
>   { rpm      :: Double
>   , bgGear   :: Int
>   , smGear   :: Int
>   , distance :: Double
>   , time     :: Double
>   } deriving Show

The number of teeth on a front chain ring or rear sprocket is an increasing
list of `Int`s, `Gears`. For our purposes, all we need to know about the
bicycle are available gears and the wheel diameter. `Trip` models the
current state of the ride. This allows us to
calculate the speed the bicycle is going given the cadence (rotations per
minute) of the pedals. In fact speed = rpm * wheelDiam * pi *  bgGear / smGear.
We will use the RWS (read, write, state) monad where the reader environment
is a `Bicycle`, the writer part is a log of the riders actions and the state
is a `Trip`.

> type Log = [String]
>
> data Ring = Big | Small deriving Show
>
> data Direction = Up | Down deriving Show
>
> type Ride a = RWS Bicycle Log Trip a

`Ring` and `Direction` types are used in the DSL to specify shifting the
front or back derailluer either up or down. A program in our DSL will
like:

> bikeRide = Ride ()
> bikeride = do
>  shift Small Down -- to a lower number of teeth
>  setRPM 90        -- set the cadence
>  go 10            -- go 10 miles at the current gear and cadence.

This will return an object of type `Ride ()` which we can evaluate with
`execTWS`. Before moving on to the DSL proper we need a few utility functions
that will be used by all of the DSL exampels we wrtie.

> nextGear :: Gears -> Int -> Int
> nextGear gs g = case elemIndex g gs of
>   Nothing -> error "Invalid Gear"
>   Just i  -> if i == length gs - 1
>              then last gs
>              else gs !! (i+1)

Given a a list `Gears` and the current gear (number of teeth), return the
new gear. If we are already at the top gear, just return it.

> prevGear :: Gears -> Int -> Int

Defined analgously to `nextGear` see the source file for details.


> speed :: Int -> Int -> Double -> Double -> Double
> speed bg sm diam r = 
>   fromIntegral bg / fromIntegral sm * diam * r * pi / (60 * 12 * 5280)

Calculate the speed where diameter is in inches, distance in miles and the
result is miles per second.

> bgRingUp :: Ride ()
> bgRingUp = do
>   bg  <- gets bgGear
>   b   <- ask
>   let cr = chainRings b
>       r  = nextGear cr bg
>   tell $ if bg == r
>     then ["Shift: *** Already at biggest gear."]
>     else ["Shift: Large ring up to " ++ show r ++ "."]
>   modify (\s -> s {bgGear = r})

> bgRingDn :: Ride ()
> smRingUp :: Ride ()
> smRingDn :: Ride ()
>

These four functions are used for shifting gears and are all defined in a
manner similar to `bgRingUp` so we only present that one. This function
demonstrates how the components of the RWS monad are utilized. We get the
current gear of the big ring with `gets`, the bicycle with `ask` and we
log the action with `tell`. Finally, we modify the state with `modify`.
