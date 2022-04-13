module BinarySearch (find) where

import Data.Array hiding ( array )
import Prelude hiding ( min, max )
import Data.Bits ( shiftR )
import Control.Applicative ( (<*>) )

find :: Ord a => Array Int a -> a -> Maybe Int
find = search <*> bounds

-- Auxiliary --

search :: Ord a => Array Int a -> (Int, Int) -> a -> Maybe Int
search array (min, max) x 
    | min > max     = Nothing
    | x < array!mid = search array (min, mid - 1) x
    | x > array!mid = search array (mid + 1, max) x
    | otherwise     = Just mid
    where 
        mid = shiftR (min + max) 1 