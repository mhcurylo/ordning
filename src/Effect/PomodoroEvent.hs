
module Effect.PomodoroEvent where

import Pomodoro.Pomodoro
import Display.Pomodoro
import Display.System
import Sound.ALUT

ignore :: IO ()
ignore = return ()

displayOnChange :: PomodoroEvent -> IO ()
displayOnChange (Change _ state) = do
  putStrLn $ clrscr <> displaySomeState state
displayOnChange _ = ignore

sound :: PomodoroEvent -> IO ()
sound (Illegal _) = playSound [Sine 480 0 0.1]
sound (Change HasAdvanced _) = playSound [Sine 280 0 0.05]
sound (Change HasFinished _) = playSound [Sine 380 0 0.5, Sine 280 0 0.5]
sound _ = ignore

runPomodoroEffects :: [PomodoroEvent] -> IO ()
runPomodoroEffects pe = do
  _ <- sequence $ [displayOnChange, sound] <*> pe
  return ()

playSound :: [SoundDataSource a] -> IO ()
playSound sounds = do
    buffers <- sequence (fmap createBuffer sounds) 
    [source] <- genObjectNames 1
    queueBuffers source buffers 
    play [source]
