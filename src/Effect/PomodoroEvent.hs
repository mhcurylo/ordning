module Effect.PomodoroEvent
  ( createSoundHandler
  , noSoundHandler
  , runPomodoroEffects
  , SoundHandler
  ) where

import Display.Pomodoro
import Display.System
import Pomodoro.Pomodoro
import Sound.ALUT


data SoundHandler = SoundHandler {
    tick :: IO ()
  , celebrate :: IO ()
  , err :: IO ()
}


noSoundHandler :: SoundHandler 
noSoundHandler = SoundHandler noSound noSound noSound
  where
  noSound = return ()

createSource :: forall a . [SoundDataSource a] -> IO Source
createSource  waves = do
  buffers <- traverse createBuffer waves
  [source] <- genObjectNames 1
  queueBuffers source buffers
  return source
  
createSoundHandler :: IO SoundHandler
createSoundHandler = do
  tickSource <- createSource [Sine 130 0 0.05]
  celebrateSource <- createSource [Sine 523 0 0.5, Sine 587 0 0.5]
  errSource <- createSource [Sine 260 0 0.1]
  return $ SoundHandler (play [tickSource]) (play [celebrateSource]) (play [errSource])

ignore :: IO ()
ignore = return ()

displayOnChange :: PomodoroEvent -> IO ()
displayOnChange (Change _ act) = putStrLn $ clrscr <> displayActivity act
displayOnChange _ = ignore

soundEffect :: SoundHandler -> PomodoroEvent -> IO ()
soundEffect sound (Illegal _) = err sound 
soundEffect sound (Change HasAdvanced _) = tick sound 
soundEffect sound (Change HasFinished _) = celebrate sound 
soundEffect _ _ = ignore

runPomodoroEffects :: SoundHandler -> [PomodoroEvent] -> IO ()
runPomodoroEffects sound = sequence_ . ([displayOnChange, soundEffect sound] <*>)

