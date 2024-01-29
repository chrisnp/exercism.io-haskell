module Acronym (abbreviate) where

import Data.Char 
import Data.Maybe 

abbreviate :: String -> String
abbreviate = mapMaybe isFirst . (zip =<< (' ' :))

isFirst :: (Char, Char) -> Maybe Char
isFirst (x, y) =
    if x == '\'' then 
        Nothing
    else if (not . isLetter) x && isLetter y then 
        Just $ toUpper y
    else if (not . isUpper) x  && isUpper y  then 
        Just y
    else 
        Nothing