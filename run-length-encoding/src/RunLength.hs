module RunLength (decode, encode) where

import Data.Char

decode :: String -> String
decode "" = ""
decode encoded =
    let 
        count = if null num then 1 else read num
        num = takeWhile isDigit encoded
        char = head (filter (not . isDigit) encoded)
    in
        replicate count char ++ decode (drop (1 + length num) encoded)

encode :: String -> String
encode "" = ""
encode text =
    let 
        len = length (takeWhile (char ==) text)
        char = head text
        lenStr = if len == 1 then "" else show len
    in
        lenStr ++ char : encode (drop len text)
        