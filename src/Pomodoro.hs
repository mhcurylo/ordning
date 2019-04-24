module Pomodoro where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad
import Control.Monad.STM
import Control.Monad.State.Strict
import Display.Pomodoro
import Display.System
import Effect.PomodoroEvent
import Options.Applicative
import Options.Configuration
import Pomodoro.Pomodoro
import Sound.ALUT
import System.IO

setUp :: IO Device
setUp = do
  putStrLn hideCursor
  (Just device) <- openDevice Nothing
  (Just context) <- createContext device []
  currentContext $= Just context
  return device

cleanUp :: Device -> IO ()
cleanUp device = do
  _ <- closeDevice device
  putStrLn displayCursor
  putStrLn $ "Bye!" <> reset

pomodoroIO :: IO ()
pomodoroIO =
  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ ->
    bracket setUp cleanUp $ \_ -> do
      (Configuration pomodoroDur shortBreakDur longBreakDur quiet) <-
        execParser configurationInfo
      case mkPomodoroConf
             (pomodoroDur * 60)
             (shortBreakDur * 60)
             (longBreakDur * 60) of
        Just config -> do
          cmds <- createCommandStream
          let someActs = someActivities config
          let current =
                withSomeActivities
                  someActs
                  (displaySomeState . toSomeState . currentActivity)
          putStrLn $ clrscr <> current
          withSomeActivities someActs (eventLoop (not quiet) cmds)
        Nothing -> putStrLn "Incorrect configuration"

eventLoop ::
     Pomodoros p s l
  => Bool
  -> TChan (Maybe PomodoroCommand)
  -> Activities p s l
  -> IO ()
eventLoop sound cmdT st = do
  cmd <- atomically $ readTChan cmdT
  case cmd of
    (Just pcmd) -> do
      let (pev, nstate) = runState (runCommand pcmd) st
      runPomodoroEffects sound pev
      eventLoop sound cmdT nstate
    Nothing -> return ()

createCommandStream :: IO (TChan (Maybe PomodoroCommand))
createCommandStream = do
  hSetBuffering stdin NoBuffering
  chan <- atomically newTChan
  _ <-
    forkIO $
    forever $ do
      threadDelay 1000000
      atomically $ writeTChan chan $ Just Advance
  _ <-
    forkIO $
    forever $ do
      c <- getChar
      case lookup c table of
        (Just cmd) -> atomically $ writeTChan chan cmd
        Nothing -> return ()
  return chan
  where
    table =
      ('q', Nothing) :
      (fmap Just <$>
       [ ('l', Next)
       , ('h', Previous)
       , ('j', Restart)
       , ('k', Start)
       , ('f', Finish)
       , ('a', Abandon)
       ])
