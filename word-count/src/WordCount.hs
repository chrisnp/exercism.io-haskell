module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.Map (Map)
import Data.List.Split (wordsBy)
import Data.MultiSet (fromList, toMap)

wordCount :: String -> Map String Int
wordCount xs = 
    let
        normalize ('\'':rest) = map toLower (init rest)
        normalize cleanword   = map toLower cleanword
    in 
        (toMap . 
         fromList . 
         map normalize . 
         wordsBy (\x -> (not . isAlphaNum) x && x /= '\'')) 
        xs