{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Freer
  ( Freer(..)
  , EffState(..)
  , put
  , get
  , update
  , runStateEff
  , etaF
  ) where

import Control.Monad


-- Freer monad as in http://okmij.org/ftp/Computation/free-monad.html
(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = (>>= g) . f

data Freer g a where
  FPure :: a -> Freer g a
  FImpure :: g x -> (x -> Freer g a) -> Freer g a

instance Functor (Freer g) where
  fmap f (FPure x) = FPure (f x)
  fmap f (FImpure u q) = FImpure u (fmap f . q)

instance Applicative (Freer g) where
  pure = FPure
  FPure f <*> x = fmap f x
  FImpure u q <*> x = FImpure u ((<*> x) . q)

instance Monad (Freer g) where
  return = FPure
  FPure x >>= k = k x
  FImpure u k' >>= k = FImpure u (k' >>> k)

etaF :: g a -> Freer g a
etaF fa = FImpure fa FPure

-- State monad as in http://okmij.org/ftp/Computation/free-monad.html
data StateEff s a where
  Get :: StateEff s s
  Put :: s -> StateEff s ()

type EffState s = Freer (StateEff s)

get :: EffState s s
get = etaF Get

put :: s -> EffState s ()
put = etaF . Put

update :: (s -> s) -> EffState s ()
update g = do
  st <- get
  put $ g st

runStateEff :: EffState s a -> s -> (a, s)
runStateEff (FPure x) s = (x, s)
runStateEff (FImpure m q) s = runStateEff (q x) s'
  where
    (x, s') = unEffState m s

unEffState :: StateEff s a -> (s -> (a, s))
unEffState Get s = (s, s)
unEffState (Put s) _ = ((), s)
