module Stream (Stream, Signal, renderSignal, loadStream) where

import Data.Int (Int32 (..))
import Data.WAVE

type Signal = Double -> Double
type Stream = [Int32]

loadStream :: String -> Int -> IO Stream
loadStream fn ch = do
   s <- getWAVEFile fn
   return $ map (!! ch) (waveSamples s)


renderSignal :: Double -> Double -> Int -> Signal -> Stream
renderSignal startPosition endPosition sampleRate s = [(toStream . clipped) (sample * period) | sample <- [0..total]]
   where toStream x = (truncate $ minSig + ((x + 1.0) / 2.0 * (maxSig - minSig)))
         minSig = fromIntegral (minBound :: Int32)
         maxSig = fromIntegral (maxBound :: Int32)
         clipped = (max (-1.0) . min 1.0 . s)
         total = (endPosition - startPosition) / period
         period = 1.0 / (fromIntegral sampleRate)
