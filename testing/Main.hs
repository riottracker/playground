import Stream
import Oscillator
import Play
import Signal
import Keys

sampleRate :: Int
sampleRate = 44100

main = do
   s <- loadStream "sample.wav" 1
   let schnipsel = take 140000 s
   playOnStdOut $ schnipsel ++ (reverse schnipsel) ++ pause
   playOnStdOut $ concat $ map (renderSignal 0.0 0.4 sampleRate) $ map (sine . keyToFreq) [40..50]
   where pause = renderSignal 0.0 0.05 sampleRate silence
