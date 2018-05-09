module PerfectNumbers (classify, Classification(..)) where

import Data.List (nub)

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n 
    | n <= 0 = Nothing
    | aliquot == n  = Just Perfect
    | aliquot > n   = Just Abundant
    | otherwise     = Just Deficient
        where 
            aliquot = aliquotSum n


factors :: Int -> [Int]
factors n = 
    let xs = semiFactors n
    in nub (1 : (xs ++ (map (n `div`) xs)))
        where semiFactors x = takeWhile (\n -> n * n <= x) [z | z <- [2..x-1], x `mod` z == 0]

aliquotSum :: Int -> Int
aliquotSum n 
    | n == 1    = 0
    | otherwise = sum ( factors n )