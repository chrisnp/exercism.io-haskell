module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = 
    filter (anagramOf xs) xss

anagramOf :: String -> String -> Bool
anagramOf a b =
    let 
        lower = map toLower
        sorted = sort . lower    
    in
        lower a /= lower b && sorted a == sorted b        