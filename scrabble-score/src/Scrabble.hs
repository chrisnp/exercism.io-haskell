module Scrabble (scoreLetter, scoreWord) where

import Data.Char

scoreLetter :: Char -> Integer
scoreLetter letter
    | letterVal ['A', 'E', 'I', 'O', 'U'] = 1
    | letterVal ['L', 'N', 'R', 'S', 'T'] = 1
    | letterVal ['D', 'G']                = 2
    | letterVal ['B', 'C', 'M', 'P']      = 3
    | letterVal ['F', 'H', 'V', 'W', 'Y'] = 4
    | letterVal ['K']                     = 5
    | letterVal ['J', 'X']                = 8
    | letterVal ['Q', 'Z']                =10
    | otherwise                           = 0
    where letterVal ofLetters = (toUpper letter) `elem` ofLetters


scoreWord :: String -> Integer
scoreWord word = sum $ map scoreLetter word
