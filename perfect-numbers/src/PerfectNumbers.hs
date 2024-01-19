module PerfectNumbers (classify, Classification(..)) where

import Data.List (nub)

data Classification = Deficient 
                    | Perfect 
                    | Abundant 
                    deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n =
    let 
        aliquot = aliquotSum n
    in 
        if n <= 0 then Nothing
        else if aliquot < n then Just Deficient
        else if aliquot > n then Just Abundant
        else Just Perfect

factors :: Int -> [Int]
factors n = 
    let
        semiFactors x = takeWhile (\y -> y * y <= x) 
                        [z | z <- [2..x-1], x `mod` z == 0]
        xs = semiFactors n
    in
        nub (1 : (xs ++ (map (n `div`) xs)))  

aliquotSum :: Int -> Int
aliquotSum n =
    case n of
        1 -> 0
        _ -> foldl (+) 0 (factors n)
