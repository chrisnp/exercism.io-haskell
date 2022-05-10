module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x = flip take triangle x

binomial :: Integer -> Integer -> Integer
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k = binomial (n-1) (k-1) * n `div` k 

triangle :: [[Integer]]
triangle = [map (binomial (x - 1)) [0..(x - 1)] | x <- [1..]]