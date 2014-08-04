{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Operational
  ( Program
  , singleton
  , ProgramView(..)
  , view
  ) where

import Control.Applicative
import Control.Monad 

data Program instr a where
  Lift  :: a -> Program instr a
  Bind  :: Program instr b -> (b -> Program instr a) -> Program instr a
  Instr :: instr a -> Program instr a

instance Monad (Program instr) where
  return = Lift
  (>>=)  = Bind

instance Functor (Program instr) where
  fmap = liftM

instance Applicative (Program instr) where
  pure  = return
  (<*>) = ap

singleton :: instr a -> Program instr a
singleton = Instr

data ProgramView instr a where
  Return :: a -> ProgramView instr a
  (:>>=) :: instr b -> (b -> Program instr a) -> ProgramView instr a

view :: Program instr a -> ProgramView instr a
view (Lift m)                = Return m
view ((Lift m)     `Bind` g) = view $ g m
view ((m `Bind` g) `Bind` h) = view (m `Bind` (\x -> g x `Bind` h))
view ((Instr i)    `Bind` g) = i :>>= g
view (Instr i)               = i :>>= return


