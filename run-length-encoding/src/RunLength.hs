module RunLength (decode, encode) where

import Data.Char

decode :: String -> String
decode encodedText
    | encodedText == "" = ""
    | otherwise         = replicate count char ++ (decode $ drop (1 + length num) encodedText) 
    where 
        count  = if null num then 1 else read num
        num    = takeWhile isDigit encodedText
        char   = head $ filter (not . isDigit) encodedText


encode :: String -> String
encode text 
    | text == "" = ""
    | otherwise  = lenStr ++ char : (encode $ drop len text)
    where 
        len    = length $ takeWhile (== char) text
        char   = head text
        lenStr = if len == 1 then "" else show len
