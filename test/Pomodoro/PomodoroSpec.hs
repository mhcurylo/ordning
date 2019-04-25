module Pomodoro.PomodoroSpec where

import Test.Hspec

import Control.Monad.State
import Pomodoro.Pomodoro

fixtureActivities :: Activities 1 1 1
fixtureActivities = activities

go ::
     forall p s l. Pomodoros p s l
  => Activities p s l
  -> [PomodoroCommand]
  -> [Activity p s l]
go acts cmds =
  fmap currentActivity $ scanl (flip $ execState . runCommand) acts cmds

progress ::
     forall p s l. Pomodoros p s l
  => Activities p s l
  -> [PomodoroCommand]
  -> Activities p s l
progress acts cmds =
  foldl (flip $ execState . runCommand) acts cmds



possibleActions ::
     forall p s l. Pomodoros p s l
  => Activities p s l
  -> [PomodoroCommand]
possibleActions acts =
  filter
    (any hasChanged . flip (evalState . runCommand) acts)
    [Previous, Next, Restart, Start, Finish, Abandon, Advance]
  where
    hasChanged :: PomodoroEvent -> Bool
    hasChanged (Change _ _) = True
    hasChanged _ = False

spec :: Spec
spec =
  describe "Pomodoro.Pomodoro.hs - Core domain" $ do
    it "Activities are in the right order going right" $
      activityActivityType <$>
      go fixtureActivities (replicate 7 Next) `shouldBe`
      [ Pomodoro
      , ShortBreak
      , Pomodoro
      , ShortBreak
      , Pomodoro
      , ShortBreak
      , Pomodoro
      , LongBreak
      ]
    it "Activities are in the right order going left" $
      activityActivityType <$>
      go fixtureActivities (replicate 8 Previous) `shouldBe`
      reverse
        [ Pomodoro
        , ShortBreak
        , Pomodoro
        , ShortBreak
        , Pomodoro
        , ShortBreak
        , Pomodoro
        , LongBreak
        , Pomodoro
        ]
    it "Pomodoro of 1 unit should finish on the second Advance" $
--  with at least 0.(9)s of error this is good enough behaviour for an ordning
      activityPhase <$>
      go fixtureActivities [Start, Advance, Advance] `shouldBe`
      [Ready, InProgress, InProgress, Finished]
    it "Ready Activity should enable commands Previous, Next, Start" $
      possibleActions fixtureActivities `shouldBe`
      [Previous, Next, Start]
    it "InProgress Activity should enable commands Finish, Abandon, Advance" $
      possibleActions (progress fixtureActivities [Start]) `shouldBe`
      [Finish, Abandon, Advance]
    it "Abandoned Activity should enable commands Previous, Next, Restart" $
      possibleActions (progress fixtureActivities [Start, Abandon]) `shouldBe`
      [Previous, Next, Restart]
    it "Finished Activity should enable commands Previous, Next" $
      possibleActions (progress fixtureActivities [Start, Advance, Advance]) `shouldBe`
      [Previous, Next]
    it "Abandoned Activity should keep its timer on Previous, Next" $
      (currentActivity $ progress fixtureActivities [Start, Advance, Abandon]) `shouldBe`
      (currentActivity $ progress fixtureActivities [Start, Advance, Abandon, Next, Previous])
    it "Abandoned Activity should forget its timer on Restart" $
      (currentActivity $ progress fixtureActivities [Start, Advance]) `shouldBe`
      (currentActivity $ progress fixtureActivities [Start, Advance, Abandon, Restart, Advance])
