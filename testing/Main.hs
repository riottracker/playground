import Stream
import Play
import Keys

sampleRate :: Int
sampleRate = 44100

type Oscillator = Double -> Signal

sine :: Oscillator
sine f t = sin $ f * 2.0 * pi * t

square :: Oscillator
square f t
  | sine f t < 0 = -1
  | otherwise = 1

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
     playOnStdOut $ renderSignal 0.0 1.0 sampleRate (major square 440)
