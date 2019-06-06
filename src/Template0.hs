module Template0 where

type MultState = (Int, Int, Int, Int)

initAState m n = (m, n, 0, 0)

evalMult :: MultState -> [MultState]
evalMult state
  | multFinal state = [state]
  | otherwise       = state : evalMult (stepMult state)

multFinal :: MultState -> Bool
multFinal (m,n,d,t) = n == 0 && d == 0 

stepMult st = case st of
  (m, n, d, t) | d > 0 -> (m, n  , d-1, t+1)
  (m, n, 0, t) | n > 0 -> (m, n-1, n,   t)
  
{-
  invariant: m * n + d + t = 6
-}
