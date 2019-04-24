module Display.System where

nl :: String
nl = "\n"

clrscr :: String
clrscr = "\x1b\x5b\x48\x1b\x5b\x32\x4a"

bright :: String
bright = "\x1b[1m"

underscore :: String
underscore = "\x1b[4m"

reset :: String
reset = "\x1b[0m"

green :: String
green = "\x1b[31m"

red :: String
red = "\x1b[32m"

hideCursor :: String
hideCursor = "\ESC[?25l"

displayCursor :: String
displayCursor = "\ESC[?25h"
