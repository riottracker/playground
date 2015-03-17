module Signal (Signal, silence, amplify, renderSignal) where

import Stream
import Data.Int (Int32 (..))

type Signal = Double -> Double

silence :: Signal
silence = const 0

amplify :: Double -> Signal -> Signal
amplify v a b = a b * v

renderSignal :: Double -> Double -> Int -> Signal -> Stream
renderSignal startPosition endPosition sampleRate s = [(toStream . clipped) (sample * period) | sample <- [0..total]]
   where toStream x = (truncate $ minSig + ((x + 1.0) / 2.0 * (maxSig - minSig)))
         minSig = fromIntegral (minBound :: Int32)
         maxSig = fromIntegral (maxBound :: Int32)
         clipped = (max (-1.0) . min 1.0 . s)
         total = (endPosition - startPosition) / period
         period = 1.0 / (fromIntegral sampleRate)
