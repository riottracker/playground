{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Block where

import Data.Tuple

class Block a where
  eval :: (a, Double) -> Buffer -> (a, Double)

data Module = forall a . (Show a, Block a) => Module a

instance Show Module where
  show (Module a) = show a

instance Block Module where
  eval (Module m, d) b = swap $ fmap Module $ swap $ eval (m,d) b

type Buffer = [(Module,Double)]

data Lambda = Lambda Lambda Lambda (Double -> Double -> Double) | Var Int | Const Double

instance Show Lambda where
  show Lambda{}   = "Lambda"
  show (Var    n) = "Var #" ++ show n
  show (Const  d) = "Const. " ++ show d

instance Block Lambda where
  eval (l,_) b = (l, evalLambda l b)

evalLambda :: Lambda -> Buffer -> Double
evalLambda (Const d) _      = d
evalLambda (Var   n) b      = snd (b !! n)
evalLambda (Lambda l r f) b = f (evalLambda l b) (evalLambda r b)

newtype Param = Param String
  deriving (Show,Eq)

instance Block Param where
  eval (p,o) _ = (p,o)
 
