module Pomodoro.Pomodoro
  ( Activities
  , Activity
  , ActivityType(..)
  , Phase(..)
  , PomodoroChangeEvent(..)
  , PomodoroCommand(..)
  , PomodoroEvent(..)
  , Pomodoros
  , activities
  , activityActivityType
  , activityPhase
  , activityTimer
  , currentActivity
  , mkSomeActivities
  , runCommand
  , withSomeActivities
  ) where

import Control.Monad.State
import Data.List (intercalate)
import Data.Proxy
import Data.Tape
import GHC.TypeLits
import Pomodoro.Timer

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

instance Show (SActivityType a) where
  show SPomodoro = "SPomodoro"
  show SShortBreak = "SShortBreak"
  show SLongBreak = "SLongBreak"

instance Eq (SActivityType a) where
  _ == _ = True

fromSActivityType :: SActivityType a -> ActivityType
fromSActivityType SPomodoro = Pomodoro
fromSActivityType SShortBreak = ShortBreak
fromSActivityType SLongBreak = LongBreak

type family ActivityTime (a :: ActivityType) (p :: Nat) (s :: Nat) (l :: Nat) :: Nat where
  ActivityTime 'Pomodoro p s l = p
  ActivityTime 'ShortBreak p s l = s
  ActivityTime 'LongBreak p s l = l

data Activity p s l where
  Activity
    :: (Pomodoros p s l, t ~ ActivityTime a p s l, TimerValue t)
    => SActivityType a
    -> Phase
    -> Timer t
    -> Activity p s l

instance Show (Activity p s l) where
  show (Activity a p t) =
    "Activity " <> intercalate ", " [show p, show t, show a]

activityActivityType :: Pomodoros p s l => Activity p s l -> ActivityType
activityActivityType (Activity a _ _) = fromSActivityType a

activityPhase :: Pomodoros p s l => Activity p s l -> Phase
activityPhase (Activity _ p _) = p

activityTimer :: Pomodoros p s l => Activity p s l -> SomeTimer
activityTimer (Activity _ _ t) = someTimer t

instance Eq (Activity p s l) where
  (Activity a1 p1 t1) == (Activity a2 p2 t2) =
    (fromSActivityType a1, p1) == (fromSActivityType a2, p2) && timerEq t1 t2

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

data PomodoroChangeEvent
  = HasStarted
  | HasAdvanced
  | HasFinished
  | IsAbandoned
  | IsSwapped

data PomodoroEvent where
  Illegal :: PomodoroCommand -> PomodoroEvent
  Change
    :: Pomodoros p s l => PomodoroChangeEvent -> Activity p s l -> PomodoroEvent

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

activities ::
     forall p s l. Pomodoros p s l
  => Activities p s l
activities = Activities $ fromLists (reverse pq) pq

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

resolve ::
     (MonadState (Activities p s l) m, Pomodoros p s l)
  => PomodoroChangeEvent
  -> Activity p s l
  -> m [PomodoroEvent]
resolve pe act = putActivity act >> return [Change pe act]

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
forceStartProgress = getCurrentActivity >>= open
  where
    startProgress :: forall a. SActivityType a -> Activity p s l
    startProgress SPomodoro = Activity SPomodoro InProgress $ timer @p
    startProgress SShortBreak = Activity SShortBreak InProgress $ timer @s
    startProgress SLongBreak = Activity SLongBreak InProgress $ timer @l
    open (Activity a _ _) = resolve HasStarted (startProgress a)

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
      return [Change IsSwapped np]
      where ns = f s
            np = currentActivity ns

runCommand ::
     forall p s l m. (MonadState (Activities p s l) m, Pomodoros p s l)
  => PomodoroCommand
  -> m [PomodoroEvent]
runCommand Next = changeActivity next Next
runCommand Previous = changeActivity previous Previous
runCommand Advance =
  getCurrentActivity >>= \case
    (Activity a InProgress t) -> resolve act np
      where (act, np) =
              case advance t of
                (Just nt) -> (HasAdvanced, Activity a InProgress nt)
                Nothing -> (HasFinished, Activity a Finished t)
    _ -> return []
runCommand Start =
  getCurrentActivity >>= \case
    (Activity _ Ready _) -> forceStartProgress
    _ -> illegal Start
runCommand Restart =
  getCurrentActivity >>= \case
    (Activity _ Abandoned _) -> forceStartProgress
    _ -> illegal Restart
runCommand Finish =
  getCurrentActivity >>= \case
    (Activity a InProgress t) -> resolve HasFinished $ Activity a Finished t
    _ -> illegal Finish
runCommand Abandon =
  getCurrentActivity >>= \case
    (Activity a InProgress t) -> resolve IsAbandoned $ Activity a Abandoned t
    _ -> illegal Abandon

someActivities :: PomodoroConf -> SomeActivities
someActivities (PomodoroConf (Proxy :: Proxy p) (Proxy :: Proxy s) (Proxy :: Proxy l)) =
  SomeActivities $ activities @p @s @l

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

mkSomeActivities :: Integer -> Integer -> Integer -> Maybe SomeActivities
mkSomeActivities pInt sInt lInt =
  someActivities <$> mkPomodoroConf pInt sInt lInt
