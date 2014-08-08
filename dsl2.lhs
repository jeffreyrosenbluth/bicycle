Deep Embedding
==============

Introduction
------------
In the previous post we showed how to embed BRL (Bicycle Ride Language) in
haskell. We did this as a shallow embedding, that is, by simply writing
haskell functions to execute the semantics of our language. In this post we
will build our first deeply embedded version of BRL. 

Abstract Syntax Tree
--------------------

Recall that our DSL was comprised of 6 functions and that a BRL program
consisted of a do block sequencing these functions. For a deep embedding
we want to create a data structure to represent an abstract syntax tree
for our language. Our first attempt will be to simply encode these functions
as data.

~~~~ { .haskell }
data Action 
  = GetTime 
  | Gears 
  | Go Double
  | Shift Ring Direction
  | GetRPM
  | SetRPM Double
~~~~

Now can can define our functions in terms of these constructors. Of course
these functions don\'t perform these actions, they simply, construct data. The
actions are executed by an interpreter, which we will need to write later.

~~~~ { .haskell }
getTime :: Action
getTime = GetTime

gears :: Action
gears = Gears

go :: Double -> Action
go = Go 

shift :: Ring -> Direction -> Action
shift = Shift

getRPM :: Action
getRPM = GetRPM

setRPM :: Double -> Action
setRPM = SetRPM
~~~~

There are some obvious problems though, `Action` can only store a single instruction.
Moreover, there is no representation of the results of an instruction, and 
hence no way to accommodate interaction between actions. We could try creating a list of
actions, but that wont solve the second problem. If we look carefully at our
shallow DSL from part 1, we realize that we are using more than 6 functions in
our language. Two more functions are hidden because we are using do notation:
return and bind. So lets see what happens if we try to include these in our data type.

~~~~ { .haskell }
data Action a
  = GetTime 
  | Gears 
  | Go Double 
  | Shift Ring Direction 
  | GetRPM 
  | SetRPM Double 
  | Return a
  | Bind (Action a) (a -> Action ?)
~~~~

This looks more promising, we can think of `Bind` as containing the current
action to be executed plus a continuation, i.e. a function that takes the
result of the first `Action` and passes it to the next action. There are still
some problems though. What do we replace ? with. Also how to we accommodate the
fact that different instructions produce results of different types, for example
`getTime` returns a `Double` but `gears` returns `(Int, Int)`. 

![](masi.jpg)

GADTs
-----
The first
problem could be solved by using the GHC existential type extension which basically
lets us use type variables on the right side of a data definition without having
it appear on the left-hand side. But to accomodate different return types we
need to use GADTs in order to refine the type `Action a`, for example to `Action Double`
or `Action (Int, Int)`. In fact, the GHC GADTs extension subsumes existential
types, so switching to a GADT will solve both problems at once! As a side
benefit the GADT notation more closely parallels the function definitions we
had in part 1. Explaining the ExistentialTypes and GADTs extensions would
take us too far afield, for Existential types see [Existentially quantified types](http://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types) for GADTs [GADTs for dummies](http://www.haskell.org/haskellwiki/GADTs_for_dummies).
Without further ado:

~~~~ { .haskell }
{-# LANGUAGE GADTs                       #-}
{-# LANGUAGE KindSignatures              #-}

data Action :: * -> * where
  GetTime  :: Action Double
  Gears    :: Action (Int, Int)
  Go       :: Double -> Action (Double, Double)
  Shift    :: Ring -> Direction -> Action ()
  GetRPM   :: Action Double
  SetRPM   :: Double -> Action Double
  Return   :: a -> Action a
  Bind     :: Action a -> (a -> Action b) -> Action b
~~~~

I should note that the `KindSignatures` extension lets us write, 
`data Action :: * -> * ` instead of `data Action a`, which I find slightly
more informative.
Of course our function type signatures must change as well:

~~~~ { .haskell }
getTime :: Action Double
gears   :: Action (Int, Int)
go      :: Double -> Action (Double, Double)
shift   :: Ring -> Direction -> Action ()
getRPM  :: Action Double
setRPM  :: Double -> Action Double
~~~~

OK, we have our data structure. The next step is to provide a monad instance
for `Action` so that we can continue to write BRL programs using do notation.


~~~~ { .haskell }
instance Monad Action where
  return = Return
  (>>=)  = Bind
~~~~

That was easy! Hold on a minute, is `Action` really monad, i.e. does it 
satisfy the monad laws. Unfortunately is does not, for example 
for the right identity law to hold we would need

~~~~ { .haskell }
go 5 >>= return === Bind (Go 5) Return
-- to be equal to 
Go 5
~~~~

which it is obviously not. This is not as dire as it seems, as long as we cannot observe
a violation of the monad laws we are OK. It does
mean that we have to be careful not to export the constructors for `Action`
and that statements like `Bind (Go 5) Return` and `Go 5` evaluate to the
save value (and produce the same side effects) in our interpreter.

Interpreter
-----------

The interpreter needs to do all of the work that our original functions did
in our shallow embedding, plus handle `Return` and `Bind`. 
We pretty much just copy and paste the bodies of
these functions into the interpreter like so.
[Bicycle2](https://github.com/jeffreyrosenbluth/bicycle/blob/master/Bicycle2.hs)

~~~~ { .haskell }
run :: Action a -> Ride a
run GetTime = gets time
run Gears = do
  bg <- gets bgGear
  sm <- gets smGear
  return (bg, sm)
run (Go dist) = do
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
run (Shift r d) = case (r, d) of
  (Big, Up)     -> bgRingUp
  (Big, Down)   -> bgRingDn
  (Small, Up)   -> smRingUp
  (Small, Down) -> smRingDn
run GetRPM = gets rpm
run (SetRPM x) = do
  tell ["Pedal: Change cadence to " ++ printf "%.2f" x]
  modify (\s -> s {rpm = x})
  return x
run (Return x)  = return x
run (Bind m@(Go x) f) =
  case f (0,0) of
    Bind (Go y) k -> run (Bind (Go (x+y)) k)
    _    -> run m >>= \z -> run (f z)
run (Bind m f) = do 
  x <- run m 
  run (f x)
~~~~

Implementing `run` on the `Bind` constructor is straight forward, we
execute the action, pass the result to the continuation and run that.
We did something a little sneaky in the `Bind Go` case,
which we had no way to do in the shallow embedding. We included a small
optimization which combines successive `Go` actions into a single action.
This is one of the advantages of a deep embedding. We can optimize the
abstract syntax tree either before or during interpretation. Another advantage
is that we could build a logging function directly from the abstract syntax
tree and avert the need for the writer monad. We won't don that here, but
it\'s a good exercise to make sure you are following.

A BRL program now produces a value of type `Action ()`, so we first need
to interpret it with `run` to get a `Ride ()`.
To run the same BRL program example from the first post we only need to
change one line of code in `Trip.hs`, 

~~~~ { .haskell }
let (s, w) = eval (run $ bikeRide 20) bike (Trip 90 36 19 0 0)
-- we simply replace bikeRide 20 with run $ bikeRide 20
~~~~

Notice that the optimization combined the two 7.5 mile legs into a single
15 mile leg! Other than that the outputs are identical.

    ---------------- Bike Trip --------------------

    Going: 2.50 miles at 13.34 mph in 11.24 minutes.
    Shift: Small ring down to 17.
    Pedal: Change cadence to 100.00
    Going: 20.00 miles at 16.57 mph in 72.42 minutes.
    Shift: Small ring down to 16.
    Going: 15.00 miles at 17.60 mph in 51.12 minutes.
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
=======

We have successfully converted our EDSL from a shallow ebedding to a deep
embedding. This opens up a whole new world of things we can do with our
language. Here we have shown how we can do some optimization and hinted at how
one could write a program that converts a BRL program to a string. But the
possibilities are really endless, once we have an abstract syntax tree we can
even write different interpreters, we could write a simulator, we could
translate our programs to another language and on and on. The only disadvantage
that we came accross was that in order to satisfy the monad laws we need to
be careful about what we export and how we interpret the syntax tree. Can we
generalize this pattern? In the next post we will show how to do this using 
operational monad.
