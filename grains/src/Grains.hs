module Grains (square, total) where

import Data.Maybe

square :: Integer -> Maybe Integer
square n = if 1 <= n && n <= 64 then Just $ (2 ^ (n-1)) else Nothing
-- square = flip (ap (if' . liftM2 (&&) (1 <=) (<= 64)) (Just . (2 ^) . subtract 1)) Nothing

total :: Integer
total = 2^64 - 1