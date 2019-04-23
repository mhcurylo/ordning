module Effect.PomodoroEvent
  ( runPomodoroEffects
  ) where

import Display.Pomodoro
import Display.System
import Pomodoro.Pomodoro
import Sound.ALUT

ignore :: IO ()
ignore = return ()

displayOnChange :: PomodoroEvent -> IO ()
displayOnChange (Change _ state) = putStrLn $ clrscr <> displaySomeState state
displayOnChange _ = ignore

sound :: PomodoroEvent -> IO ()
sound (Illegal _) = playSound [Sine 480 0 0.1]
sound (Change HasAdvanced _) = playSound [Sine 280 0 0.05]
sound (Change HasFinished _) = playSound [Sine 380 0 0.5, Sine 280 0 0.5]
sound _ = ignore

runPomodoroEffects :: [PomodoroEvent] -> IO ()
runPomodoroEffects = sequence_ . ([displayOnChange, sound] <*>)

playSound :: [SoundDataSource a] -> IO ()
playSound sounds = do
  buffers <- traverse createBuffer sounds
  [source] <- genObjectNames 1
  queueBuffers source buffers
  play [source]
