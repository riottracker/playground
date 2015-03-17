module Stream (Stream, loadStream) where

import Data.Int (Int32 (..))
import Data.WAVE

type Stream = [Int32]

loadStream :: String -> Int -> IO Stream
loadStream fn ch = do
   s <- getWAVEFile fn
   return $ map (!! ch) (waveSamples s)
