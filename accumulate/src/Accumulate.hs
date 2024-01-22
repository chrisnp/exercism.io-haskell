module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate op collection =
    case collection of
        []     -> []
        (x:xs) -> op x : accumulate op xs