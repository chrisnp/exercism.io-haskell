module Sublist (sublist) where

import Data.List ( isInfixOf )


sublist :: (Eq a) => [a] -> [a] -> Maybe Ordering
sublist xs ys 
    | xs == ys = Just EQ
    | flip isInfixOf ys xs = Just LT
    | flip isInfixOf xs ys = Just GT
    | otherwise = Nothing