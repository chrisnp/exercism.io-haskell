module Atbash (decode, encode) where
import Data.Char
import Control.Monad (liftM2)

encode :: String -> String
encode = 
    let 
        valid = liftM2 (&&) isAlphaNum isAscii
    in 
        chunkify 5 . map atbash . filter valid

decode :: String -> String
decode = map atbash . filter (liftM2 (||) isAlpha isDigit)

chunkify :: Int -> String -> String
chunkify x text = 
    let 
        (chunk, rest) = splitAt x text
    in 
        if null rest then chunk 
        else chunk ++ " " ++ chunkify x rest

atbash :: Char -> Char
atbash x
    | isAlpha x = chr . (-) (ord 'a' + ord 'z') . ord . toLower $ x
    | otherwise = id x
