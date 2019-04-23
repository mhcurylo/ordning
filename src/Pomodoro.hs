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
import Pomodoro.Pomodoro
import Sound.ALUT
import System.IO

setUp :: IO Device
setUp = do
  (Just device) <- openDevice Nothing
  (Just context) <- createContext device []
  currentContext $= Just context
  return device

cleanUp :: Device -> IO ()
cleanUp device = do
  _ <- closeDevice device
  putStrLn $ clrscr <> "Bye!" <> reset

pomodoroIO :: IO ()
pomodoroIO =
  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ ->
    bracket setUp cleanUp $ \_ ->
      case mkPomodoroConf 1500 300 900 of
        Just config -> do
          cmds <- createCommandStream
          let someActs = someActivities config
          let current =
                withSomeActivities
                  someActs
                  (displaySomeState . toSomeState . currentActivity)
          putStrLn $ clrscr <> current
          withSomeActivities someActs (eventLoop cmds)
        Nothing -> putStrLn "Incorrect configuration"

eventLoop ::
     Pomodoros p s l
  => TChan (Maybe PomodoroCommand)
  -> Activities p s l
  -> IO ()
eventLoop cmdT st = do
  cmd <- atomically $ readTChan cmdT
  case cmd of
    (Just pcmd) -> do
      let (pev, nstate) = runState (runCommand pcmd) st
      runPomodoroEffects pev
      eventLoop cmdT nstate
    Nothing -> return ()

createCommandStream :: IO (TChan (Maybe PomodoroCommand))
createCommandStream = do
  hSetBuffering stdin NoBuffering
  putStrLn noblinking
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
