{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Display.System where

nl :: String
nl = "\n"

clrscr :: String
clrscr = "\x1b\x5b\x48\x1b\x5b\x32\x4a"

bright     = "\x1b[1m"
underscore = "\x1b[4m"
reset      = "\x1b[0m"
green      = "\x1b[31m"
red        = "\x1b[32m" 
noblinking  = "\ESC[?25l"

