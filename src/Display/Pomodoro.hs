module Display.Pomodoro
  ( displaySomeState
  ) where

import Display.System
import Display.Timer
import Pomodoro.Pomodoro

displayActivityType :: ActivityType -> String
displayActivityType Pomodoro = "Pomodoro"
displayActivityType ShortBreak = "Short break"
displayActivityType LongBreak = "Long break"

displaySomeState :: SomeState -> String
displaySomeState (SomeState a t) =
  displayActivityType a <> nl <> displaySomeTimer t
