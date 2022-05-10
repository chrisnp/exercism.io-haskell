module Grains (square, total) where

import Control.Monad ( liftM2 )
import Control.Applicative ( (<*>) )
import Data.Bool ( bool )

square :: (Integral a, Num a) => a -> Maybe a
square = (bool Nothing . Just . (2 ^) . subtract 1) 
         <*> 
         (liftM2 (&&) (1 <=) (<= 64))

total :: (Num a, Integral a) => a
total = 2^64 - 1
