module Pomodoro.Pomodoro (
    Activities(..)
  , Phase
  , Pomodoros
  , PomodoroCommand(..)
  , PomodoroChangeEvent(..)
  , PomodoroEvent(..)
  , ActivityType(..)
  , SomeState(..)
  , mkPomodoroConf
  , someActivities
  , withSomeActivities
  , toSomeState
  , currentActivity
  , runCommand
  ) where

import Data.Tape
import Pomodoro.Timer

import Control.Monad.State
import Data.Proxy
import GHC.TypeLits

class (TimerValue p, TimerValue s, TimerValue l) =>
      Pomodoros p s l


instance (TimerValue p, TimerValue s, TimerValue l) => Pomodoros p s l

data Phase
  = Ready
  | Finished
  | Abandoned
  | InProgress
  deriving (Show, Eq)

data ActivityType
  = Pomodoro
  | LongBreak
  | ShortBreak
  deriving (Show, Eq)

data SActivityType (a :: ActivityType) where
  SPomodoro :: SActivityType 'Pomodoro
  SShortBreak :: SActivityType 'ShortBreak
  SLongBreak :: SActivityType 'LongBreak

data SomeSActivityType where
  SomeSActivityType :: SActivityType a -> SomeSActivityType

fromSActivityType :: SActivityType a -> ActivityType
fromSActivityType SPomodoro = Pomodoro
fromSActivityType SShortBreak = ShortBreak
fromSActivityType SLongBreak = LongBreak

withSomeSActivityType ::
     SomeSActivityType
  -> (forall (a :: ActivityType). SActivityType a -> r)
  -> r
withSomeSActivityType (SomeSActivityType a) f = f a

type family ActivityTimer (a :: ActivityType) (p :: Nat) (s :: Nat) (l :: Nat) :: Nat where
  ActivityTimer 'Pomodoro p s l = p
  ActivityTimer 'ShortBreak p s l = s
  ActivityTimer 'LongBreak p s l = l

data Activity p s l where
  Activity
    :: (Pomodoros p s l, t ~ ActivityTimer a p s l, TimerValue t)
    => (SActivityType a)
    -> Phase
    -> Timer t
    -> Activity p s l

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
  , someStatePhase :: Phase
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
toSomeState (Activity pa p t) = SomeState (fromSActivityType pa) p $ someTimer t

data Activities p s l where
  Activities :: Pomodoros p s l => Tape (Activity p s l) -> Activities p s l

previous :: Activities p s l -> Activities p s l
previous (Activities tape) = Activities (left tape)

next :: Activities p s l -> Activities p s l
next (Activities tape) = Activities (right tape)

currentActivity :: Activities p s l -> Activity p s l
currentActivity (Activities tape) = value tape

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
  [ pomodoro
  , shortBreak
  , pomodoro
  , shortBreak
  , pomodoro
  , shortBreak
  , pomodoro
  , longBreak
  ]
  where
    pomodoro = Activity SPomodoro Ready (timer @p)
    shortBreak = Activity SShortBreak Ready (timer @s)
    longBreak = Activity SLongBreak Ready (timer @l)

illegal ::
     (MonadState (Activities p s l) m, Pomodoros p s l)
  => PomodoroCommand
  -> m [PomodoroEvent]
illegal c = return [Illegal c]

getCurrentActivity ::
     (MonadState (Activities p s l) m, Pomodoros p s l) => m (Activity p s l)
getCurrentActivity = gets currentActivity

putActivity ::
     (MonadState (Activities p s l) m, Pomodoros p s l)
  => Activity p s l
  -> m ()
putActivity act = modify (swapActivity act)

forceStartProgress ::
     forall p s l m. (MonadState (Activities p s l) m, Pomodoros p s l)
  => m [PomodoroEvent]
forceStartProgress = do
  act <- getCurrentActivity
  open act
  where
    startProgress :: SActivityType a -> Activity p s l
    startProgress SPomodoro = Activity SPomodoro InProgress $ timer @p
    startProgress SShortBreak = Activity SShortBreak InProgress $ timer @s
    startProgress SLongBreak = Activity SLongBreak InProgress $ timer @l
    open (Activity a _ _) = do
      putActivity np
      return [Change HasStarted (toSomeState np)]
      where
        np = withSomeSActivityType (SomeSActivityType a) startProgress

changeActivity ::
     forall p s l m. (MonadState (Activities p s l) m, Pomodoros p s l)
  => (Activities p s l -> Activities p s l)
  -> PomodoroCommand
  -> m [PomodoroEvent]
changeActivity f pc = do
  s <- get
  let p = currentActivity s
  case p of
    (Activity _ InProgress _) -> illegal pc
    _ -> do
      put ns
      return [Change IsSwapped (toSomeState np)]
      where ns = f s
            np = currentActivity ns

runCommand ::
     forall p s l m. (MonadState (Activities p s l) m, Pomodoros p s l)
  => PomodoroCommand
  -> m [PomodoroEvent]
runCommand Next = changeActivity next Next
runCommand Previous = changeActivity previous Previous
runCommand Advance = do
  p <- getCurrentActivity
  case p of
    (Activity a InProgress t) -> do
      putActivity np
      return [Change act (toSomeState np)]
      where (act, np) =
              case advance t of
                (Just nt) -> (HasAdvanced, Activity a InProgress nt)
                Nothing -> (HasFinished, Activity a Finished t)
    _ -> return []
runCommand Start = do
  act <- getCurrentActivity
  case act of
    (Activity _ Ready _) -> forceStartProgress
    _ -> illegal Start
runCommand Restart = do
  act <- getCurrentActivity
  case act of
    (Activity _ Abandoned _) -> forceStartProgress
    _ -> illegal Restart
runCommand Finish = do
  p <- getCurrentActivity
  case p of
    (Activity a InProgress t) -> do
      putActivity np
      return [Change HasFinished (toSomeState np)]
      where np = Activity a Finished t
    _ -> illegal Finish
runCommand Abandon = do
  s <- get
  let p = currentActivity s
  case p of
    (Activity a InProgress t) -> do
      put (swapActivity np s)
      return [Change IsAbandoned (toSomeState np)]
      where np = Activity a Abandoned t
    _ -> illegal Abandon

someActivities :: PomodoroConf -> SomeActivities
someActivities (PomodoroConf (Proxy :: Proxy p) (Proxy :: Proxy s) (Proxy :: Proxy l)) =
  SomeActivities $ pomodoroTape @p @s @l

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
