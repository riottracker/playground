module Oscillator (Oscillator, sine, sawtooth, triangle, square) where

import Signal

type Oscillator = Double -> Signal

sine :: Oscillator
sine f t = sin $ f * 2.0 * pi * t

sawtooth :: Oscillator
sawtooth f t = 2 * (f*t - fromInteger (floor $ 0.5 + f*t))

triangle :: Oscillator
triangle f t = 2 * abs (sawtooth f t) - 1

square :: Oscillator
square f t
   | sine f t < 0 = -1
   | otherwise = 1
