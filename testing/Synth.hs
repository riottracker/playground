{-# LANGUAGE RecordWildCards #-}

module Synth (Synth(..), Envelope(..), Oscillator(..), bell, envelope, trackEnvelope, sine, square, quint, octave, minor, major) where

import Stream
import Instrument
import Keys
import Event

type Oscillator = Double -> Signal

data Synth = Synth
           { osc     :: Oscillator
           , env     :: Envelope
           , elapsed :: Double
           , tone    :: Maybe Int
           }

instance Instrument Synth where
  handleEvent s NoteOff    = s { elapsed = 0, tone = Nothing }
  handleEvent s (NoteOn n) = s { elapsed = 0, tone = Just n }
  render s dur sampleRate  = (i, renderSignal (elapsed s) (elapsed i) sampleRate synth)
    where i = s { elapsed = (elapsed s) + dur }
          synth = case tone s of
                     Just n -> trackEnvelope (osc s) (env s) $ keyToFreq n
                     Nothing -> silence




data Envelope = Envelope
              { attack       :: Double
              , decay        :: Double
              , sustain      :: Double
              , sustainLevel :: Double
              , release      :: Double
              }

-- TODO: make this logarithmic
envelope :: Envelope -> Double -> Double
envelope Envelope{..} t
  | t <= attack                             = (t / attack)
  | t <= attack + decay                     = 1.0 - (t - attack) / decay * (1.0 - sustainLevel)
  | t <= attack + decay + sustain           = sustainLevel
  | t <= attack + decay + sustain + release = sustainLevel - sustainLevel * (t - attack - decay - sustain) / release
  | otherwise                               = 0


sine :: Oscillator
sine f t = sin $ f * 2.0 * pi * t

square :: Oscillator
square f t
  | sine f t < 0 = -1
  | otherwise = 1

trackEnvelope :: Oscillator -> Envelope -> Oscillator
trackEnvelope o e = \f t -> (o f t) * (envelope e t)

mix :: Oscillator -> Oscillator -> Oscillator
mix o1 o2 = \f t -> (o1 f t + o2 f t) * 0.5

quint :: Oscillator -> Oscillator
quint o = mix o (o . ((3/2)*))

octave :: Oscillator -> Oscillator
octave o = mix o (o . ((1/2)*))

minor :: Oscillator -> Oscillator
minor o = mix (quint o) (o . ((6/5)*))

major :: Oscillator -> Oscillator
major o = mix (quint o) (o . ((5/4)*))

bell = Synth (octave sine) (Envelope 0.05 0.04 0.2 0.2 1.0) 0.0 Nothing

