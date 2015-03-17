module Keys () where

-- this is an awful makeshift solution

keyToFreq :: Double -> Double
keyToFreq k = 440.0*2**((k-49.0)/12)
