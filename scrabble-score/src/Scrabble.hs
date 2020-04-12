module Scrabble (scoreLetter, 
                 scoreWord) where

import Data.Char

scoreLetter :: Char -> Integer
scoreLetter letter
    | letterVal "AEIOU" = 1
    | letterVal "LNRST" = 1
    | letterVal "DG"    = 2
    | letterVal "BCMP"  = 3
    | letterVal "FHVWY" = 4
    | letterVal "K"     = 5
    | letterVal "JX"    = 8
    | letterVal "QZ"    = 10
    | otherwise         = 0
    where 
        letterVal = 
            (elem . toUpper) letter


scoreWord :: String -> Integer
scoreWord = 
    sum . fmap scoreLetter 