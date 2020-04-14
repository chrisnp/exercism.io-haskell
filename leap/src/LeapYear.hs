module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year
    | divby 100 = divby 400
    | otherwise = divby 4
    where 
        divby x = (==) 0 (mod year x)