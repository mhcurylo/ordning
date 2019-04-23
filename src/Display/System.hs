{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Display.System where

nl :: String
nl = "\n"

clrscr :: String
clrscr = "\x1b\x5b\x48\x1b\x5b\x32\x4a"

bright :: [Char]
bright     = "\x1b[1m"
underscore :: [Char]
underscore = "\x1b[4m"
reset :: [Char]
reset      = "\x1b[0m"
green :: [Char]
green      = "\x1b[31m"
red :: [Char]
red        = "\x1b[32m" 
noblinking :: [Char]
noblinking  = "\ESC[?25l"

