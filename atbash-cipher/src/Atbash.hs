module Atbash (decode, encode) where

import Data.Char

chunkify :: Int -> String -> String
chunkify n text = 
    let 
        (chunk, rest) = splitAt n text
    in 
        if null rest then chunk 
        else chunk ++ " " ++ chunkify n rest

atbash :: Char -> Char
atbash x
    | isAlpha x = chr (ord 'a' + ord 'z' - ord (toLower x))
    | otherwise = x

decode :: String -> String
decode cipherText = (map atbash . filter (\ ch -> 
                                    isAlpha ch || isDigit ch)) 
                    cipherText

encode :: String -> String
encode plainText = 
    let 
        valid x = isAlphaNum x && isAscii x
    in 
        (chunkify 5 . map atbash . filter valid) plainText