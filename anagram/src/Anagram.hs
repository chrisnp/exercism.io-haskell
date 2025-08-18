module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor = filter . anagramOf

anagramOf :: String -> String -> Bool
anagramOf =
    let 
        lower = map toLower
        sorted = sort . lower    
    in
        ((<*>) . ((&&) .) . (. lower) . (/=) . lower) 
        <*> 
        ((. sorted) . (==) . sorted)
    {-  
        i.e. (for arguments \x and \y)
        x anagramOf y iff lower x /= lower y && sorted x == sorted y
    -}
