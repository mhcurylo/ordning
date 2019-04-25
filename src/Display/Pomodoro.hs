module Display.Pomodoro
  ( displayActivity
  ) where

import Display.System
import Display.Timer
import Pomodoro.Pomodoro

displayActivityType :: ActivityType -> String
displayActivityType Pomodoro = "Pomodoro"
displayActivityType ShortBreak = "Short break"
displayActivityType LongBreak = "Long break"

seperateLimit :: Char -> Int -> String -> String -> String
seperateLimit c t s1 s2 = s1 <> replicate (t - length s1 - length s2) c <> s2

displayActivity :: forall p s l . Pomodoros p s l => Activity p s l  -> String
displayActivity act =
  seperateLimit ' ' 23 (displayActivityType . activityActivityType $ act) (show . activityPhase $ act) <> nl <>
  displaySomeTimer (activityTimer act)
