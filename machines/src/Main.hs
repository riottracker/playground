{-# LANGUAGE RankNTypes #-}

module Main where

import           Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Machine as M
import           Data.Int (Int32 (..))

sampleRate :: Int
sampleRate = 44100

oscillator :: Double -> (Double -> Double) -> M.Source Double
oscillator freq w = M.source [ t * (1.0 / fromIntegral sampleRate) | t <- [1..100000] ]

tremolo :: M.Tee Double Double Double
tremolo = M.zipWith (*)

c :: Double -> Int32
c x = (truncate $ minSig + ((clipped + 1.0) / 2.0 * (maxSig - minSig)))
 where clipped = (max (-1.0) . min 1.0 $ x)
       minSig  = fromIntegral (minBound :: Int32)
       maxSig  = fromIntegral (maxBound :: Int32)


output = M.mapping c

example = output M.<~ M.tee (oscillator 400 sin) (oscillator 40 sin) tremolo

main :: IO ()
main = B.putStr . encode $ M.run example
