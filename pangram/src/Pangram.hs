module Pangram (isPangram) where

import Data.Char (toLower, isLetter)
import Data.List


-- filterAppeared :: String -> String
-- filterAppeared [] = []
-- filterAppeared (x:xs)
--     | isAlpha x = x:(filterAppeared $ filter (/=x) xs)
--     | otherwise = filterAppeared xs


isPangram :: String -> Bool
isPangram text = length (group $ sort $ map toLower $ filter isLetter text) == 26
