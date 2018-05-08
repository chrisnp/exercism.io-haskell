module Acronym (abbreviate) where

import Data.Char 
import Data.Maybe 

abbreviate :: String -> String
abbreviate xs = mapMaybe isFirst $ zip (' ':xs) xs
    where
        isFirst (x, y) 
            | (not . isLetter) x && isLetter y = Just $ toUpper y
            | (not . isUpper) x  && isUpper y  = Just y
            | otherwise                        = Nothing 