# Ordning

An infinite list of pomodoro activities with timers and some sound.

Inspired by IKEA.

## To install

Needs OpenAL / alut installed.

https://wiki.haskell.org/ALUT

Install with stack.

## To enjoy

Type ordning, then input commands:

l - next activity

h - previous activity

k - start activity

f - finish activity

a - abandon activity 

j - restart abandoned activity

q - quit

# Command line options:

To see command line options run with ordning -h.

Available options:

  -p,--pomodoro INTEGER    The duration of a pomodoro in minutes (default: 25)

  -s,--shortBrake INTEGER  The duration of a short brake in minutes (default: 5)

  -l,--longBrake INTEGER   The duration of a long brake in minutes (default: 5)

  -q,--quiet               Turn the sound off

  -h,--help                Show this help text

