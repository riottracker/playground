{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad.ST
import           Data.Array.ST
import           Data.STRef
import           Data.Binary
import qualified Data.ByteString.Lazy as B
import           Data.Int (Int8 (..))
import           Block

net :: ST s (Network s)
net = do
  a <- newListArray (0,0) [Module (Sine $ Const 440, 0)]
  n <- newSTRef 0
  return $ Network a (Var 0) [] n

doubleToSample :: Double -> Int8
doubleToSample v =
    let maxb = fromIntegral (maxBound :: Int8)
        minb = fromIntegral (minBound :: Int8) in
    if v >= 0
    then (fromIntegral . floor . (* maxb)) (min v 1)
    else (fromIntegral . ceiling . (* minb)) (min (-v) 1)

render :: Int -> Network s -> ST s [Double]
render 0   _ = return []
render n net@Network{..} = do
    net' <- step net
    frame <- evalLambda sink buffer
    frames <- render (n-1) net'
    return $ frame:frames 
   
main :: IO ()
main = mapM_ print $ runST (net >>= (render $ sampleRate * 2))
 

