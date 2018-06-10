module Base (Error(..), rebase) where

import Data.List     (unfoldr)
import Data.Tuple    (swap)
import Control.Monad (foldM)


data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a deriving (Show, Eq)

validDigit :: (Ord a, Num a) => a -> a -> Bool
validDigit base d = d >= 0 && d < base

from :: (Foldable t, Ord p, Num p) => p -> t p -> Either (Error p) p 
from base = foldM f 0 
    where f acc x | validDigit base x = Right (acc * base + x) 
                  | otherwise = Left (InvalidDigit x)

to :: Integral t => t -> t -> [t]
to base = reverse . unfoldr f 
    where f x | x == 0 = Nothing 
              | otherwise = Just . swap $ x `divMod` base

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2  = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | otherwise      = to outputBase <$> from inputBase inputDigits



