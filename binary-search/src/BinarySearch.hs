module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = binsearch array (bounds array) x

binsearch :: Ord a => Array Int a -> (Int, Int) -> a -> Maybe Int
binsearch array (min, max) x 
    | min > max     = Nothing
    | x < array!mid = binsearch array (min, mid - 1) x
    | x > array!mid = binsearch array (mid + 1, max) x
    | otherwise     = Just mid
    where 
        mid = (min + max) `div` 2