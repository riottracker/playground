{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Block where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Tuple
import Debug.Trace (traceM)

sampleRate :: Int
sampleRate = 44100

class Block a where
  eval :: Buffer s -> Int -> (a, Double) -> ST s (a, Double)

data Module = forall a . (Show a, Block a) => Module (a,Double)

output :: Module -> Double
output (Module (a,b)) = b

instance Show Module where
  show (Module (a,b)) = show (a,b)

type Buffer s = STArray s Int Module

data Network s = Network { buffer :: Buffer s
                         , sink   :: Lambda
                         , params :: [Int]
                         , t      :: STRef s Int
                         }

evalElem :: Buffer s -> Int -> Int -> ST s Module
evalElem b t n = readArray b n >>= \(Module e) -> Module <$> eval b t e
 
step :: Network s -> ST s (Network s)
step n@Network{..} = do
   k <- readSTRef t
   mapM_ (evalElem buffer k) =<< range <$> getBounds buffer
   modifySTRef' t (+1)
   return n

data Lambda = Lambda Lambda Lambda (Double -> Double -> Double) | Var Int | Const Double

instance Show Lambda where
  show Lambda{}   = "Lambda"
  show (Var    n) = "Var #" ++ show n
  show (Const  d) = "Const. " ++ show d

instance Block Lambda where
  eval b _ (l,_) = (l ,) <$> evalLambda l b

evalLambda :: Lambda -> Buffer s -> ST s Double
evalLambda (Const d) _      = return d
evalLambda (Var   n) b      = output <$> readArray b n
evalLambda (Lambda l r f) b = liftM2 f (evalLambda l b) (evalLambda r b)

newtype Param = Param String
  deriving (Show,Eq)

instance Block Param where
  eval _ _ (p,o) = return (p,o)

data Sine = Sine { freq :: Lambda }
  deriving (Show)

instance Block Sine where
    eval b t (Sine freq, _) = do
      f <- evalLambda freq b
      return (Sine freq, fromIntegral t)
      --return (Sine freq, sin $ pi * 2.0 * f / fromIntegral sampleRate)
