module Pomodoro.PomodoroSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.State
import Data.List
import Pomodoro.Pomodoro
import Pomodoro.Timer

fixtureActivites :: Activities 25 5 15
fixtureActivites = activities

go ::
     forall p s l . Pomodoros p s l
  => Activities p s l
  -> [PomodoroCommand]
  -> [Activities p s l]
go = scanr (execState . runCommand) 


spec :: Spec
spec =
  describe "Pomodoro.Pomodoro.hs - Core domain" $ do
    it
      "Is testable." $
        (currentActivity fixtureActivites) `shouldBe` (currentActivity fixtureActivites)
