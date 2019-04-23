module Display.Pomodoro
  ( displaySomeState
  ) where

import Display.System
import Display.Timer
import Pomodoro.Pomodoro

displayActivityType :: ActivityType -> String
displayActivityType Pomodoro = "Pomodoro"
displayActivityType ShortBrake = "Short brake"
displayActivityType LongBrake = "Long brake"

displaySomeState :: SomeState -> String
displaySomeState (SomeState a t) =
  displayActivityType a <> nl <> displaySomeTimer t
