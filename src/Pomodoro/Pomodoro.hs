{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Pomodoro.Pomodoro where

import Data.Freer
import Data.Tape
import Pomodoro.Timer

import Data.Proxy

class (TimerValue p, TimerValue s, TimerValue l) =>
      Pomodoros p s l


instance (TimerValue p, TimerValue s, TimerValue l) => Pomodoros p s l

data Phase where
  Ready :: Phase
  Finished :: Phase
  Abandoned :: TimerValue p => Timer p -> Phase
  InProgress :: TimerValue p => Timer p -> Phase

data ActivityType
  = Pomodoro
  | LongBrake
  | ShortBrake
  deriving (Show, Eq)

data Activity p s l where
  Activity :: Pomodoros p s l => ActivityType -> Phase -> Activity p s l

data PomodoroConf where
  PomodoroConf
    :: Pomodoros p s l => Proxy p -> Proxy s -> Proxy l -> PomodoroConf

data PomodoroCommand
  = Next
  | Previous
  | Restart
  | Start
  | Finish
  | Abandon
  | Advance
  deriving (Show, Eq)

data SomeState = SomeState
  { someStateActivityType :: ActivityType
  , someStateTimer :: SomeTimer
  }

data PomodoroChangeEvent
  = HasStarted
  | HasAdvanced
  | HasFinished
  | IsAbandoned
  | IsSwapped

data PomodoroEvent
  = Illegal { illegalCommand :: PomodoroCommand }
  | Change { changeEvent :: PomodoroChangeEvent
           , changeState :: SomeState }

toSomeState ::
     forall p s l. Pomodoros p s l
  => Activity p s l
  -> SomeState
toSomeState (Activity Pomodoro Ready) =
  SomeState Pomodoro $ someTimer $ timer @p
toSomeState (Activity ShortBrake Ready) =
  SomeState ShortBrake $ someTimer $ timer @s
toSomeState (Activity LongBrake Ready) =
  SomeState LongBrake $ someTimer $ timer @l
toSomeState (Activity Pomodoro Finished) =
  SomeState Pomodoro $ someTimer $ finishedTimer @p
toSomeState (Activity ShortBrake Finished) =
  SomeState ShortBrake $ someTimer $ finishedTimer @s
toSomeState (Activity LongBrake Finished) =
  SomeState LongBrake $ someTimer $ finishedTimer @l
toSomeState (Activity a (Abandoned t)) = SomeState a $ someTimer t
toSomeState (Activity a (InProgress t)) = SomeState a $ someTimer t

data Activities p s l where
  Activities :: Pomodoros p s l => Tape (Activity p s l) -> Activities p s l

previous :: Activities p s l -> Activities p s l
previous (Activities tape) = Activities (left tape)

next :: Activities p s l -> Activities p s l
next (Activities tape) = Activities (right tape)

currentActivity :: Activities p s l -> Activity p s l
currentActivity (Activities tape) = value tape

modifyActivity ::
     Pomodoros p s l
  => (Activity p s l -> Activity p s l)
  -> Activities p s l
  -> Activities p s l
modifyActivity f (Activities tape) = Activities ntape
  where
    ntape = swapValue (f . value $ tape) tape

swapActivity :: Activity p s l -> Activities p s l -> Activities p s l
swapActivity act (Activities tape) = Activities $ swapValue act tape

data SomeActivities where
  SomeActivities :: Pomodoros p s l => Activities p s l -> SomeActivities

withSomeActivities ::
     SomeActivities
  -> (forall p s l. Pomodoros p s l =>
                      Activities p s l -> r)
  -> r
withSomeActivities (SomeActivities acts) f = f acts

pomodoroTape ::
     forall p s l. Pomodoros p s l
  => Activities p s l
pomodoroTape = Activities $ fromLists (reverse pq) pq

pq ::
     forall p s l. Pomodoros p s l
  => [Activity p s l]
pq =
  [ Activity Pomodoro Ready
  , Activity ShortBrake Ready
  , Activity Pomodoro Ready
  , Activity ShortBrake Ready
  , Activity Pomodoro Ready
  , Activity LongBrake Ready
  ]

illegal ::
     Pomodoros p s l
  => PomodoroCommand
  -> EffState (Activities p s l) [PomodoroEvent]
illegal c = return [Illegal c]

getCurrentActivity :: EffState (Activities p s l) (Activity p s l)
getCurrentActivity = fmap currentActivity get

putActivity :: Activity p s l -> EffState (Activities p s l) ()
putActivity act = update (\s -> swapActivity act s)

forceStartProgress ::
     forall p s l. Pomodoros p s l
  => EffState (Activities p s l) [PomodoroEvent]
forceStartProgress = do
  act <- getCurrentActivity
  open act
  where
    startProgress Pomodoro = InProgress $ timer @p
    startProgress LongBrake = InProgress $ timer @l
    startProgress ShortBrake = InProgress $ timer @s
    open (Activity a _) = do
      putActivity np
      return [Change HasStarted (toSomeState np)]
      where
        np = Activity a $ startProgress a

changeActivity ::
     forall p s l. Pomodoros p s l
  => (Activities p s l -> Activities p s l)
  -> PomodoroCommand
  -> EffState (Activities p s l) [PomodoroEvent]
changeActivity f pc = do
  s <- get
  let p = currentActivity s
  case p of
    (Activity _ (InProgress _)) -> illegal pc
    _ -> do
      put ns
      return [Change IsSwapped (toSomeState np)]
      where ns = f s
            np = currentActivity ns

runCommand ::
     forall p s l. Pomodoros p s l
  => PomodoroCommand
  -> EffState (Activities p s l) [PomodoroEvent]
runCommand Next = changeActivity next Next
runCommand Previous = changeActivity previous Previous
runCommand Advance = do
  p <- getCurrentActivity
  case p of
    (Activity a (InProgress t)) -> do
      putActivity np
      return [Change act (toSomeState np)]
      where (act, np) =
              case advance t of
                (Just nt) -> (HasAdvanced, Activity a $ InProgress nt)
                Nothing -> (HasFinished, Activity a Finished)
    _ -> return []
runCommand Start = do
  act <- getCurrentActivity
  case act of
    (Activity _ Ready) -> forceStartProgress
    _ -> illegal Start
runCommand Restart = do
  act <- getCurrentActivity
  case act of
    (Activity _ (Abandoned _)) -> forceStartProgress
    _ -> illegal Restart
runCommand Finish = do
  p <- getCurrentActivity
  case p of
    (Activity a (InProgress _)) -> do
      putActivity np
      return [Change HasFinished (toSomeState np)]
      where np = Activity a Finished
    _ -> illegal Finish
runCommand Abandon = do
  s <- get
  let p = currentActivity s
  case p of
    (Activity a (InProgress t)) -> do
      put (swapActivity np s)
      return [Change IsAbandoned (toSomeState np)]
      where np = Activity a (Abandoned t)
    _ -> illegal Abandon

someActivities :: PomodoroConf -> SomeActivities
someActivities (PomodoroConf (Proxy :: Proxy p) (Proxy :: Proxy s) (Proxy :: Proxy l)) =
  SomeActivities $ pomodoroTape @p @s @l

defaultConf :: PomodoroConf
defaultConf =
  PomodoroConf
    (Proxy :: (Proxy 1500))
    (Proxy :: (Proxy 300))
    (Proxy :: (Proxy 900))

mkPomodoroConfWithSomeTimerValues ::
     SomeTimerValue -> SomeTimerValue -> SomeTimerValue -> PomodoroConf
mkPomodoroConfWithSomeTimerValues p s l =
  withSomeTimerValue l $
  withSomeTimerValue s $ withSomeTimerValue p PomodoroConf

mkPomodoroConf :: Integer -> Integer -> Integer -> Maybe PomodoroConf
mkPomodoroConf pInt sInt lInt =
  mkPomodoroConfWithSomeTimerValues <$> someTimerValue pInt <*>
  someTimerValue sInt <*>
  someTimerValue lInt
