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
                , tone    :: Maybe Int
                }

type Song = [Maybe Int]

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

silence :: Signal
silence = const 0

exampleSong = [ Just 67, Nothing, Nothing, Just 65, Nothing, Nothing, Nothing, Nothing
              , Just 65, Nothing, Nothing, Just 64, Nothing, Nothing, Nothing, Nothing
              , Just 63, Nothing, Nothing, Just 62, Nothing, Nothing, Nothing, Just 58
              , Just 58, Just 57, Just 60, Just 48, Nothing, Nothing, Nothing, Nothing
              ]

play :: Song -> Int -> Instrument -> IO ()
play s pos ins = p >>= \i -> if pos + 1 == length s then return ()
                             else play s (pos + 1) i
  where p = case s !! pos of
              Just n -> do
                 playOnStdOut $ renderSignal 0.0 0.3 sampleRate $ trackEnvelope (osc ins) (env ins) $ keyToFreq n
                 return $ ins { elapsed = 0.3, tone = Just n }
              Nothing -> do
                case tone ins of
                  Just n -> do
                              playOnStdOut $ renderSignal (elapsed ins) (elapsed ins + 0.3) sampleRate $
                                             trackEnvelope (osc ins) (env ins) $ keyToFreq n
                              return $ ins { elapsed = (elapsed ins) + 0.3}
                  Nothing -> do
                              playOnStdOut $ renderSignal 0.0 0.3 sampleRate silence
                              return ins

main = do
--   s <- loadStream "sample.wav" 1
--   let schnipsel = take 140000 s
--   playOnStdOut $ schnipsel ++ (reverse schnipsel) ++ pause
--   playOnStdOut $ concat $ map (renderSignal 0.0 0.4 sampleRate) $ map (sine . keyToFreq) [40..50]
-- where pause = renderSignal 0.0 0.05 sampleRate silence
     let bell = Instrument (octave sine) (Envelope 0.05 0.04 0.2 0.2 1.0) 0.0 Nothing
     play exampleSong 0 bell
