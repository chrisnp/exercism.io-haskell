module Grains (square, total) where

import Control.Monad ( liftM2 )
import Control.Applicative ( (<*>) )

square :: (Integral a, Num a) => a -> Maybe a
square = flip 
    ((if' . liftM2 (&&) (1 <=) (<= 64)) <*> (Just . (2 ^) . subtract 1)) 
    Nothing

total :: (Num a, Integral a) => a
total = 2^64 - 1

-- Auxiliary --

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y