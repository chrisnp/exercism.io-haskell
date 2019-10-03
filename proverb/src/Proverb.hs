module Proverb(recite) where

import Text.Printf

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zipWith (,) xs (tail xs)

premise :: (String, String) -> String
premise (x, y) = printf "For want of a %s the %s was lost.\n" x y

conclusion :: String -> String
conclusion x = printf "And all for the want of a %s." x

recite :: [String] -> String
recite [] = []
recite xs = concat $ verses ++ [epilogue]
    where
        verses =  map premise (pairs xs)
        epilogue = conclusion $ head xs