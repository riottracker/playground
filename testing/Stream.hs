module Stream (Stream, Signal, renderSignal) where
--, loadStream) where

import Data.Int (Int32 (..))
--import Data.WAVE

type Signal = Double -> Double
type Stream = [Int32]

--loadStream :: String -> Int -> IO Stream
--loadStream fn ch = do
--   s <- getWAVEFile fn
--   return $ map (!! ch) (waveSamples s)


renderSignal :: Double -> Double -> Int -> Signal -> Stream
renderSignal startPosition endPosition sampleRate s = [(toStream . clipped) (sample * period) | sample <- [start..end]]
   where toStream x = (truncate $ minSig + ((x + 1.0) / 2.0 * (maxSig - minSig)))
         minSig = fromIntegral (minBound :: Int32)
         maxSig = fromIntegral (maxBound :: Int32)
         clipped = (max (-1.0) . min 1.0 . s)
         start = startPosition / period
         end = endPosition / period
         period = 1.0 / (fromIntegral sampleRate)
