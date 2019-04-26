# Ordning

An infinite list of pomodoro activities with timers and some sound.

Inspired by IKEA and the book Thinking with Types.

## The why

Pomodoro needs some sound.

Developer needs a side project.

## To install

Get OpenAL / ALUT installed.
```
https://wiki.haskell.org/ALUT
```
Install with stack. 
```
https://docs.haskellstack.org/en/stable/README/#how-to-install
```


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

```
ordning -h

Available options:

  -p,--pomodoro INTEGER    The duration of a pomodoro in minutes (default: 25)

  -s,--shortBreak INTEGER  The duration of a short break in minutes (default: 5)

  -l,--longBreak INTEGER   The duration of a long break in minutes (default: 5)

  -q,--quiet               Turn the sound off

  -h,--help                Show this help text
```
