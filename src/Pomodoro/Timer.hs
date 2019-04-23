module Pomodoro.Timer
  ( Timer
  , TimerValue
  , SomeTimerValue
  , SomeTimer
  , timer
  , advance
  , finishedTimer
  , timerValue
  , timerMax
  , someTimer
  , withSomeTimer
  , someTimerValue
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
  MkSomeTimer :: (TimerValue a) => Timer a -> SomeTimer

someTimer :: (TimerValue a) => Timer a -> SomeTimer
someTimer = MkSomeTimer

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
withSomeTimer (MkSomeTimer v) f = f v

withSomeTimerValue ::
     SomeTimerValue
  -> (forall a. (TimerValue a) =>
                  Proxy a -> r)
  -> r
withSomeTimerValue (MkSomeTimerValue v) f = f v

instance Show (Timer a) where
  show (MkTimer f) = "Timer " ++ show f

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
