module Instrument (Instrument (..)) where

import Event
import Stream


class Instrument i where
  render :: i -> Double -> Int -> (i, Stream)
  handleEvent :: i -> Event -> i


