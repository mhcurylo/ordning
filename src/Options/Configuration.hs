module Options.Configuration
  ( Configuration(..)
  , configurationInfo
  ) where

import Data.Semigroup ((<>))
import Options.Applicative

data Configuration = Configuration
  { confPomodoroDuration :: Integer
  , confShortBreakDuration :: Integer
  , confLongBreakDuration :: Integer
  , confQuiet :: Bool
  }

configurationInfo :: ParserInfo Configuration
configurationInfo =
  info (configurationParser <**> helper) (fullDesc <> progDesc "The pomodoro timer for the command line." <> header "Ordning")

configurationParser :: Parser Configuration
configurationParser =
  Configuration <$>
  option
    auto
    (long "pomodoro" <> short 'p' <>
     help "The duration of a pomodoro in minutes" <>
     showDefault <>
     value 25 <>
     metavar "INTEGER") <*>
  option
    auto
    (long "shortBreak" <> short 's' <>
     help "The duration of a short break in minutes" <>
     showDefault <>
     value 5 <>
     metavar "INTEGER") <*>
  option
    auto
    (long "longBreak" <> short 'l' <>
     help "The duration of a long break in minutes" <>
     showDefault <>
     value 15 <>
     metavar "INTEGER") <*>
  switch (long "quiet" <> short 'q' <> help "Turn the sound off")
