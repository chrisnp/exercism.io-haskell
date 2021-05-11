module Acronym (abbreviate) where

import Data.Char 
import Data.Maybe 


isFirst :: (Char, Char) -> Maybe Char
isFirst (x, y)
    | x == '\'' = Nothing
    | (not . isLetter) x && isLetter y = Just $ toUpper y
    | (not . isUpper) x  && isUpper y = Just y
    | otherwise = Nothing

abbreviate :: String -> String
abbreviate = mapMaybe isFirst . (zip =<< (' ' :))
