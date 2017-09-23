{-# LANGUAGE RankNTypes #-}

module Main where

import           Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Machine as M
import qualified Data.Machine.Mealy as M
import           Data.Int (Int16 (..))

sampleRate :: Int
sampleRate = 44100

oscillator :: Double -> (Double -> Double) -> M.Source Double
oscillator freq w = M.source [ w $ t * pi * 2.0 * (1.0 / fromIntegral sampleRate) | t <- [1.. fromIntegral sampleRate*10] ]

tremolo :: M.Tee Double Double Double
tremolo = M.zipWith (*)

c :: Double -> Int16
c x = (truncate $ minSig + ((clipped + 1.0) / 2.0 * (maxSig - minSig)))
 where clipped = (max (-1.0) . min 1.0 $ x)
       minSig  = fromIntegral (minBound :: Int16)
       maxSig  = fromIntegral (maxBound :: Int16)


output = M.mapping c

--example = output M.<~ M.tee (oscillator 100 sin) (oscillator 400 sin) tremolo
example = output M.<~ (M.auto $ filt 640 0.0) M.<~ (oscillator 100 sin)

filt :: Double -> Double -> M.Mealy Double Double
filt cutoff res = M.unfoldMealy (vcf cutoff res) (0,0,0,0,0)

type FilterState = (Double, Double, Double, Double, Double)

vcf :: Double -> Double -> FilterState -> Double -> (Double, FilterState)
vcf cutoff res (x,y1,y2,y3,y4) input = (out, (x',y1',y2',y3',y4'))
  where x' = input - r*y4
        y1' = p*(x' + x)   - k*y1
        y2' = p*(y1 + y1') - x*y2
        y3' = p*(y2 + y2') - x*y3
        y4' = p*(y3 + y3') - x*y4
        out = y4 - (y4*y4*y4)/6.0
        f  = 2 * cutoff / 44100
        k  = 3.6 * f - 1.6*f*f - 1
        p  = (k+1)*0.5
        scale = (exp 1)**((1-p)*1.4)
        r  = res * scale
  
main :: IO ()
main = B.putStr . encode $ M.run example

