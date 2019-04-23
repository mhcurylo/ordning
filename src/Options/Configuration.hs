module Options.Configuration
  ( Configuration(..)
  , configurationInfo
  ) where

import Data.Semigroup ((<>))
import Options.Applicative

data Configuration = Configuration
  { confPomodoroDuration :: Integer
  , confShortBrakeDuration :: Integer
  , confLongBrakeDuration :: Integer
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
    (long "shortBrake" <> short 's' <>
     help "The duration of a short brake in minutes" <>
     showDefault <>
     value 5 <>
     metavar "INTEGER") <*>
  option
    auto
    (long "longBrake" <> short 'l' <>
     help "The duration of a long brake in minutes" <>
     showDefault <>
     value 15 <>
     metavar "INTEGER") <*>
  switch (long "quiet" <> short 'q' <> help "Turn the sound off")
