module Pomodoro.Timer
  ( SomeTimer
  , SomeTimerValue
  , Timer
  , TimerValue
  , advance
  , finishedTimer
  , someTimer
  , someTimerValue
  , timer
  , timerEq
  , timerMax
  , timerValue
  , withSomeTimer
  , withSomeTimerValue
  ) where

import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits
import GHC.TypeLits.Compare

import Data.Fin

class (KnownNat a) =>
      TimerValue (a :: Nat)


instance (KnownNat a, a <= 5400) => TimerValue a

data Timer a =
  (TimerValue a) =>
  MkTimer (Fin a)

data SomeTimer where
  SomeTimer :: (TimerValue a) => Timer a -> SomeTimer

instance Eq SomeTimer where
  (SomeTimer t1) == (SomeTimer t2) = t1 `timerEq` t2

someTimer :: (TimerValue a) => Timer a -> SomeTimer
someTimer = SomeTimer

data SomeTimerValue where
  MkSomeTimerValue :: (TimerValue a) => Proxy a -> SomeTimerValue

someTimerValue :: Integer -> Maybe SomeTimerValue
someTimerValue i =
  case someNatVal i of
    Just (SomeNat v) ->
      case v %<=? Proxy @5400 of
        LE Refl -> Just $ MkSomeTimerValue v
        NLE _ _ -> Nothing
    Nothing -> Nothing

withSomeTimer ::
     SomeTimer
  -> (forall a. (TimerValue a) =>
                  Timer a -> r)
  -> r
withSomeTimer (SomeTimer v) f = f v

withSomeTimerValue ::
     SomeTimerValue
  -> (forall a. (TimerValue a) =>
                  Proxy a -> r)
  -> r
withSomeTimerValue (MkSomeTimerValue v) f = f v

instance Show (Timer a) where
  show (MkTimer f) = "Timer " ++ show f

instance Eq (Timer a) where
  (MkTimer a) == (MkTimer b) = a == b

timerEq :: (TimerValue a, TimerValue b) => Timer a -> Timer b -> Bool
timerEq t1 t2 = timerMax t1 == timerMax t1 && timerValue t1 == timerValue t2

timer ::
     forall a. (TimerValue a)
  => Timer a
timer = MkTimer maxBound

finishedTimer ::
     forall a. (TimerValue a)
  => Timer a
finishedTimer = MkTimer minBound

advance :: Timer a -> Maybe (Timer a)
advance (MkTimer f) = MkTimer <$> predecessor f

timerMax ::
     forall (a :: Nat). KnownNat a
  => Timer a
  -> Integer
timerMax _ = natVal (Proxy @a)

timerValue :: Timer a -> Integer
timerValue (MkTimer f) = finToInteger f
