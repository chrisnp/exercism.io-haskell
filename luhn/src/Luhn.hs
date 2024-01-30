module Luhn (isValid) where

import Data.Char (digitToInt)

isValid :: String -> Bool
isValid n = 
    let
        checksum = (`mod` 10) . luhn . reverse . map digitToInt
        luhn []  = 0
        luhn [x] = x
        luhn (x0:x1:xs) = x0 + 2 * x1 - (if x1 >= 5 then 9 else 0) + (luhn xs)
    in
        case filter (/= ' ') n of
            []  -> False
            [_] -> False
            x   -> checksum x == 0
