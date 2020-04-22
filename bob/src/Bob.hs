module Bob (responseFor) where

import Data.Char (isUpper, isAlpha, isSpace)

isQuestion :: String -> Bool
isQuestion xs
    | null xs           = False
    | last xs == '?'    = True
    | isSpace $ last xs = isQuestion $ init xs
    | otherwise         = False

isShouting :: String -> Bool
isShouting xs 
    | null listWords || null listUpperWords = False
    | listWords == listUpperWords           = True
    | otherwise                             = False
        where
            listWords = filter isAlpha xs
            listUpperWords = filter isUpper xs

isSilent :: String -> Bool
isSilent xs = null . filter (not . isSpace) $ xs

responseFor :: String -> String
responseFor xs
    | question && shout = "Calm down, I know what I'm doing!" 
    | silent            = "Fine. Be that way!"
    | shout             = "Whoa, chill out!"
    | question          = "Sure."
    | otherwise         = "Whatever."
        where 
            silent   = isSilent xs
            shout    = isShouting xs
            question = isQuestion xs