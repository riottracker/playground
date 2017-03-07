{-# LANGUAGE RecordWildCards #-}

import Stream
import Play
import Keys

sampleRate :: Int
sampleRate = 44100

type Oscillator = Double -> Signal

data Envelope = Envelope
              { attack       :: Double
              , decay        :: Double
              , sustain      :: Double
              , sustainLevel :: Double
              , release      :: Double
              }

data Instrument = Instrument
                { osc     :: Oscillator
                , env     :: Envelope
                , elapsed :: Double
                }

data Event = NoteOn  Int Int
           | NoteOff Int Int

type Song = [Maybe [Event]]

data Player = Player
            { song        :: Song
            , position    :: Int
            , instruments :: [Instrument]
            } 

envelope :: Envelope -> Double -> Double
envelope Envelope{..} t
  | t <= attack                             = (t / attack)
  | t <= attack + decay                     = 1.0 - ((t - attack) / decay) * (1.0 - sustainLevel)
  | t <= attack + decay + sustain           = sustainLevel
  | t <= attack + decay + sustain + release = sustainLevel - (sustainLevel * (t - (attack + decay + sustain))/release)
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

minor :: Oscillator -> Oscillator
minor o = mix (quint o) (o . ((6/5)*))

major :: Oscillator -> Oscillator
major o = mix (quint o) (o . ((5/4)*))


main = do
--   s <- loadStream "sample.wav" 1
--   let schnipsel = take 140000 s
--   playOnStdOut $ schnipsel ++ (reverse schnipsel) ++ pause
--   playOnStdOut $ concat $ map (renderSignal 0.0 0.4 sampleRate) $ map (sine . keyToFreq) [40..50]
-- where pause = renderSignal 0.0 0.05 sampleRate silence
     let e1 = Envelope 0.5 0.5 0.5 0.2 0.5
     playOnStdOut $ renderSignal 0.0 2.0 sampleRate $ trackEnvelope (major sine) e1 $ 440
