module FactFunc where

fact :: Num a => Int -> a
fact 0 = 1
fact n = fromIntegral n * fact (n - 1)
