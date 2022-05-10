module LeapYear (isLeapYear) where

import Data.Bool ( bool )

isLeapYear :: Integer -> Bool
isLeapYear year =
    let 
        divby = (0 ==) . mod year
    in 
        bool (divby 4) (divby 400) (divby 100)
