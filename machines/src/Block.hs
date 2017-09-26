{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}

module Block where

import Data.Tuple

sampleRate :: Int
sampleRate = 44100

class Block a where
  eval :: (a, Double) -> Buffer -> Int -> (a, Double)

data Module = forall a . (Show a, Block a) => Module (a,Double)

output :: Module -> Double
output (Module (a,b)) = b

instance Show Module where
  show (Module (a,b)) = show (a,b)

type Buffer = [Module]

data Network = Network { buffer :: Buffer
                       , sink   :: Lambda
                       , params :: [Int]
                       , t      :: Int
                       }

step :: Network -> (Network, Double)
step n@Network{..} =
  (n { buffer = fmap (\(Module a) -> Module $ eval a buffer (t+1)) buffer
     , t = t + 1
     }
  , evalLambda sink buffer
  )

data Lambda = Lambda Lambda Lambda (Double -> Double -> Double) | Var Int | Const Double

instance Show Lambda where
  show Lambda{}   = "Lambda"
  show (Var    n) = "Var #" ++ show n
  show (Const  d) = "Const. " ++ show d

instance Block Lambda where
  eval (l,_) b _ = (l, evalLambda l b)

evalLambda :: Lambda -> Buffer -> Double
evalLambda (Const d) _      = d
evalLambda (Var   n) b      = output (b !! n)
evalLambda (Lambda l r f) b = f (evalLambda l b) (evalLambda r b)

newtype Param = Param String
  deriving (Show,Eq)

instance Block Param where
  eval (p,o) _ _ = (p,o)

data Sine = Sine { freq :: Lambda }
  deriving (Show)

instance Block Sine where
  eval (Sine freq, _) b t =
      (Sine freq, sin $ pi * 2.0 * f / fromIntegral sampleRate)
    where f = evalLambda freq b


