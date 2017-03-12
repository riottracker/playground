{-# LANGUAGE ExistentialQuantification #-}

module Instrument (Instrument (..), Instrument_ (..)) where

import Event
import Stream

import Control.Arrow

class Instrument_ i where
  render      :: Double -> Int -> i -> (i, Stream)
  handleEvent :: Event -> i -> i

data Instrument = forall a. (Instrument_ a) => Instrument a

instance Instrument_ Instrument where
  render duration sampleRate (Instrument x) = first Instrument (render duration sampleRate x)
  handleEvent ev (Instrument x) = Instrument (handleEvent ev x)
