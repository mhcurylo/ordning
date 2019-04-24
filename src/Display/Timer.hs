module Display.Timer
  ( displaySomeTimer
  , displayTimer
  ) where

import Display.System
import GHC.TypeLits
import Pomodoro.Timer

type Padding = Integer

type Max = Integer

type At = Integer

prepend :: Padding -> Char -> String -> String
prepend n ch xs
  | n < 1 = xs
  | otherwise = prepend (n - 1) ch (ch : xs)

leftPad :: Padding -> Char -> String -> String
leftPad n ch str = prepend i ch str
  where
    i = n - fromIntegral (length str)

ordningTape :: Max -> String
ordningTape m =
  concat $
  map
    (leftPad 2 ' ' .
     (\x ->
        if (x `mod` 5) == 0
          then show x
          else "--"))
    [0 .. (m `div` 2)] ++
  repeat "  "

surroundAt :: Integer -> [a] -> [a] -> [a] -> [a]
surroundAt _ a b [] = a ++ b
surroundAt 0 a b [x] = a ++ [x] ++ b
surroundAt 0 a b (x:y:xs) = a ++ [x, y] ++ b ++ xs
surroundAt n a b (x:xs) = x : surroundAt (n - 1) a b xs

around :: At -> Padding -> Max -> String
around a p m =
  (<> reset <> hideCursor) $
  (green <>) $
  surroundAt p (bright <> red) (reset <> hideCursor <> green) .
  leftPad (p * 2 + 1) ' ' .
  drop (fromInteger $ a - p) . take (fromInteger $ p + a + 1) $
  ordningTape m

colorClock :: Padding -> At -> Max -> String
colorClock p a m = "[" ++ around a p m ++ "]"

displayTimer :: KnownNat a => Timer a -> String
displayTimer t =
  colorClock 10 ((timerValue t + 29) `div` 30) (timerMax t `div` 30)

displaySomeTimer :: SomeTimer -> String
displaySomeTimer st = withSomeTimer st displayTimer
