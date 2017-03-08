module Keys (keyToFreq) where

-- this is an awful makeshift solution
-- maybe we could use something similar to Haskore.Basic.Pitch

keyToFreq :: Int -> Double
keyToFreq k = 440.0*2**(((fromIntegral k)-49.0)/12)
