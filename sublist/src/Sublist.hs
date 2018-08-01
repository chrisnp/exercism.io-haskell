module Sublist (Sublist(..), sublist) where

import Data.List

data Sublist = Equal | Sublist | Superlist | Unequal deriving (Eq, Show)

sublist :: (Eq a) => [a] -> [a] -> Sublist
sublist xs ys 
    | xs == ys             = Equal
    | flip isInfixOf ys xs = Sublist
    | flip isInfixOf xs ys = Superlist
    | otherwise            = Unequal