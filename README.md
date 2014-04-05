# Haskell 2048
A clone of the popular _2048_ game originally by Gabriele Cirulli.

This clone serves the sole purpose of reminding me of practical Haskell concepts for my Functional Programming exam.

## Prerequisites
    cabal install random
    cabal install mtl

## Issues
* The score is not yet recorded
* Using getChar only works in ghci, in compiled standalone executable it waits for a line break/EOF.
* Game over condition is not triggered before attempting to add a new tile, causing a division by zero.
