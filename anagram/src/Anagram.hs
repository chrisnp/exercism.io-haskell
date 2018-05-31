module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (anagramOf xs) xss

anagramOf :: String -> String -> Bool
anagramOf a b 
    | lower a /= lower b && sorted a == sorted b = True
    | otherwise = False  
    where 
        lower = map toLower
        sorted = sort . lower
