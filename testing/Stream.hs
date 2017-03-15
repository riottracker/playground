module Stream (Stream, Signal, renderFrame, renderSignal, silence) where

import Data.Int (Int32 (..))

type Signal = Double -> Double
type Stream = [Int32]

silence :: Signal
silence = const 0

renderFrame :: Int -> Signal -> Int -> Int32
renderFrame sampleRate s position = toStream . clipped $ (fromIntegral position) * period
   where toStream x = (truncate $ minSig + ((x + 1.0) / 2.0 * (maxSig - minSig)))
         minSig = fromIntegral (minBound :: Int32)
         maxSig = fromIntegral (maxBound :: Int32)
         clipped = (max (-1.0) . min 1.0 . s)
         period = 1.0 / (fromIntegral sampleRate)
 

renderSignal :: Double -> Double -> Int -> Signal -> Stream
renderSignal startPosition endPosition sampleRate s = [ renderFrame sampleRate s $ floor  sample | sample <- [start..end] ]
   where start = startPosition / period
         end = endPosition / period
         period = 1.0 / (fromIntegral sampleRate)
