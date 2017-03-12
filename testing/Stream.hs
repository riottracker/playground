module Stream (Stream, Signal, renderSignal, silence) where

import Data.Int (Int32 (..))

type Signal = Double -> Double
type Stream = [Int32]

silence :: Signal
silence = const 0

renderSignal :: Double -> Double -> Int -> Signal -> Stream
renderSignal startPosition endPosition sampleRate s = [(toStream . clipped) (sample * period) | sample <- [start..end]]
   where toStream x = (truncate $ minSig + ((x + 1.0) / 2.0 * (maxSig - minSig)))
         minSig = fromIntegral (minBound :: Int32)
         maxSig = fromIntegral (maxBound :: Int32)
         clipped = (max (-1.0) . min 1.0 . s)
         start = startPosition / period
         end = endPosition / period
         period = 1.0 / (fromIntegral sampleRate)
