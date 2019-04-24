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

seperateLimit :: Char -> Int -> String -> String -> String
seperateLimit c t s1 s2 = s1 <> take (t - length s1 - length s2) (repeat c) <> s2

displaySomeState :: SomeState -> String
displaySomeState (SomeState a p t) =
  seperateLimit ' ' 23 (displayActivityType a) (show p) <> nl <>
  displaySomeTimer t
