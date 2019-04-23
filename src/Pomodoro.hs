{-# Language GeneralizedNewtypeDeriving #-}
{-# Language GADTs #-}

module Pomodoro where

import Control.Monad
import Display.System
import System.IO
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Exception
import Pomodoro.Pomodoro
import Display.Pomodoro
import Effect.PomodoroEvent
import Sound.ALUT
import Control.Monad.State.Strict

setUp :: IO Device
setUp = do 
  (Just device) <- openDevice Nothing
  (Just context) <- createContext device []
  currentContext $= Just context
  return device

cleanUp :: Device -> IO ()
cleanUp device = do
  _ <- closeDevice device
  putStrLn $ reset <> clrscr <> "Bye!"

pomodoroIO :: IO ()
pomodoroIO = withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> 
  bracket setUp cleanUp $ \_ ->
    case mkPomodoroConf 1500 300 900 of
      Just config -> do
        cmds <- createCommandStream
        let someActs = someActivities config
        let current = withSomeActivities someActs (displaySomeState . toSomeState . currentActivity)
        putStrLn $ clrscr <> current
        withSomeActivities someActs (eventLoop cmds)
      Nothing -> do
        putStrLn "Incorrect configuration"

eventLoop :: Pomodoros p s l => TChan (Maybe PomodoroCommand) -> Activities p s l -> IO ()
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
  chan <- atomically $ newTChan
  _ <- forkIO $ forever $ do
    threadDelay 1000000
    atomically $ do
      writeTChan chan $ Just Advance
  _ <- forkIO $ forever $ do
    c <- getChar
    case lookup c table of
      (Just cmd) -> atomically $ writeTChan chan cmd
      Nothing -> return ()
  return chan
  where
    table = ('q', Nothing) : (fmap (fmap Just) $ [('l', Next),('h', Previous), ('j', Restart), ('k', Start), ('f', Finish), ('a', Abandon)])
