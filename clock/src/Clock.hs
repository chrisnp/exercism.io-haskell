module Clock (fromHourMin, addDelta, toString) where

data Clock = Clock { hours :: Int, minutes :: Int } deriving Eq

instance Show Clock where
    show = toString

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = Clock hour' min'
    where
        hour' = (hour + dh) `mod` 24
        min'  = minute `mod` 60
        dh    = minute `div` 60 

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minute clock =
    fromHourMin (hour + hours clock) (minute + minutes clock)

toString :: Clock -> String
toString clock = padTime (hours clock) ++ ":" ++ padTime (minutes clock)
        where
            padTime x 
                | x < 10    = '0' : show x
                | otherwise = show x