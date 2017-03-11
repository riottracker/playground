#!/bin/sh

runhaskell Main.hs | play -traw -B -r 44100 -e signed -b 32 -
