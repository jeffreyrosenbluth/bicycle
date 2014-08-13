The Operational Monad
=====================

Introduction
------------

Last time we converted our shallow embedding of BRL to a deep embedding.
Now we are going to think about how to generalize what we did. In this
post we will use a strategy that eventually leads to the operational monad.
In the next post, our design will produce the Free monad. It turns out
that they are isomorphic, but that\'s another story, see [reddit/r/haskell - Edward Kmett](http://www.reddit.com/r/haskell/comments/29wyr8/is_there_a_relation_between_the_operational_monad/).

![](operation.jpg)

Strategy
--------

Let\'s refresh our memories a bit. Here is the data structure we wound up
with last time that encodes our abstract syntax tree,


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

If we want to abstract this pattern, we will want to get `Return` and  `Bind`
out of the data structure so that: 

1. It contains only instruction  unique to our DSL.
2. Users of the abstraction don't have to reimplement `bind` and `return` every 
   time they implement it.

It may seem strange that we
want to remove `Bind` and `Return` after we spent most of the previous post
adding them. But we don't really want to get rid of them just handle them
differently. The strategy we employ here is to create a new data type `Program`
which contains constructors for both `Bind` and `Return`, and to wrap our revised
`Action` type in it, i.e `Program ActionO a`. For a different 
approach to deriving and motivating the operational monad see
[The Operational Monad Tutorial](http://apfelmus.nfshost.com/articles/operational-monad.html).

Our revised algebraic data type which we call `ActionO` (O for operational) looks
like,

~~~~ { .haskell }
data ActionO :: * -> * where
  GetTime :: ActionO Double
  Gears :: ActionO (Int, Int)
  Go :: Double -> ActionO (Double, Double)
  Shift :: Ring -> Direction -> ActionO ()
  GetRPM :: ActionO Double
  SetRPM :: Double -> ActionO Double
~~~~

and our new data type `Program` is,

~~~~ { .haskell }
data Program :: (* -> *) -> * -> * where
  Return :: a -> Program instr a
  Bind   :: Program instr a -> (a -> Program instr b) -> Program instr b
  Instr  :: instr a -> Program instr a
~~~~

We need the `Instr` constructor so that our `Program` type can hold `ActionO` s,
and since both `ActionO` and `instr` are of  kind `* -> *` we\'re covered.
Although the signature `data Program :: (* -> *) -> * -> *` is equivalent
to `data Program instr a ` we continue to use the kind signature because
`(* -> *)` makes it clear that `instr` is a data contructor. Next we create

~~~~ { .haskell }
type Action = Program ActionO

instance Monad (Program instr) where
  return = Return
  (>>=)  = Bind
~~~~

Eventually we wont want to export the data constructors for `Program`, as
 a first step we'll create a function to to make actions into `Program instr`s.
Then our DSL functions will be,

~~~~ { .haskell }
singleton :: instr a -> Program instr a
singleton = Instr

getTime :: Action Double
getTime = singleton GetTime

gears :: Action (Int, Int)
gears = singleton Gears

go :: Double -> Action (Double, Double)
go = singleton . Go

shift :: Ring -> Direction -> Action ()
shift r d = singleton $ Shift r d

getRPM :: Action Double
getRPM = singleton  GetRPM

setRPM :: Double -> Action Double
setRPM = singleton . SetRPM 
~~~~

Now all of the work in the interpreter is done in the `Bind` cases. You can
think of `(Instr GetTime) Bind k)` as get the elapsed time and then do `k`.
Where `k` represents the remaining actions.

~~~~ { .haskell }
run :: Action a -> Ride a
run (Return x)              = return x
run (Instr x)               = run ((Instr x) `Bind` Return)
run ((m `Bind` g) `Bind` h) = run $ m `Bind` (\x -> g x `Bind` h)
run ((Return x) `Bind` k)   = run (k x)

run ((Instr GetTime) `Bind` k) = do
  tm <- gets time
  run (k tm)
run ((Instr Gears) `Bind` k) = do
  bg <- gets bgGear
  sm <- gets smGear
  run (k (bg, sm))
run ((Instr (Go dist)) `Bind` k) = do
  case k (0,0) of
    (Instr (Go dist') `Bind` k') ->
      run (go (dist + dist') >>= k')
    _                -> do
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
      run (k (sp, tm))
run ((Instr (Shift r d)) `Bind` k) = do
  case (r, d) of
    (Big, Up)     -> do 
      tell ["Shift: Large ring up."] 
      bgRingUp
    (Big, Down)   -> bgRingDn
    (Small, Up)   -> smRingUp
    (Small, Down) -> smRingDn
  run (k ())
run ((Instr GetRPM) `Bind` k) = do
  c <- gets rpm
  run (k c)
run ((Instr (SetRPM x)) `Bind` k) = do
  tell ["Pedal: Change cadence to " ++ printf "%.2f" x]
  modify (\s -> s {rpm = x})
  run (k x)
~~~~

The full code is here,
[Bicycle7](https://github.com/jeffreyrosenbluth/bicycle/blob/master/Bicycle7.hs)

One potential problem we still have is that we only satisfy the monad laws
observationally. But now we have the means to enforce this. The trick, as 
alluded to above, is not to expose the `Programs` data constructors. In
order to do this we need to give the DSL writer a way to build the interpreter.
Following the [operational](http://hackage.haskell.org/package/operational) package
we define,

~~~~ { .haskell }
data ProgramView instr a where
  Return :: a -> ProgramView instr a
  (:>>=) :: instr b -> (b -> Program instr a) -> ProgramView instr a

view :: Program instr a -> ProgramView instr a
view (Lift m)                = Return m
view ((Lift m)     `Bind` g) = view $ g m
view ((m `Bind` g) `Bind` h) = view (m `Bind` (\x -> g x `Bind` h))
view ((Instr i)    `Bind` g) = i :>>= g
view (Instr i)               = i :>>= return
~~~~

Which allows us to apply `view` to a program and then pattern match on
`(:>>=)`. In addition to protecting the DSL writer from violating the
monad laws, our interpreter code is cleaner in that the "boring" cases
are handled by `view`.

~~~~ { .haskell }
run :: Action a -> Ride a
run m = case view m of
  Return x -> return x
  GetTime :>>= k -> do
    tm <-gets time
    run (k tm)
  Gears :>>= k -> do
    bg <- gets bgGear
    sm <- gets smGear
    run (k (bg, sm))
  Go dist :>>= k -> 
    case view $ k (0,0) of
      Go dist' :>>= k' ->
        run (go (dist + dist') >>= k')
      _                -> do
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
        run (k (sp, tm))
  Shift r d :>>= k -> do
    case (r, d) of
      (Big, Up)     -> bgRingUp
      (Big, Down)   -> bgRingDn
      (Small, Up)   -> smRingUp
      (Small, Down) -> smRingDn
    run (k ())
  GetRPM :>>= k -> do
    c <- gets rpm
    run (k c)
  SetRPM x :>>= k -> do
    tell ["Pedal: Change cadence to " ++ printf "%.2f" x]
    modify (\s -> s {rpm = x})
    run (k x)
~~~~

See the full source in, 
[Bicycle6](https://github.com/jeffreyrosenbluth/bicycle/blob/master/Bicycle6.hs)
and [Operational](https://github.com/jeffreyrosenbluth/bicycle/blob/master/Operational.hs)
