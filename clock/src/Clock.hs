module Clock (fromHourMin,
              addDelta,
              toString) 
    where

data Clock = 
    Clock { hours :: Int, minutes :: Int } 
    deriving Eq

instance Num Clock where
    (Clock h1 m1) + (Clock h2 m2) = 
        fromHourMin ( h1 + h2 ) ( m1 + m2 )
    (Clock h1 m1) - (Clock h2 m2) = 
        fromHourMin ( h1 - h2 ) ( m1 - m2 )
    fromInteger = 
        fromHourMin 0 . fromIntegral 
    abs _ = undefined
    signum _ = undefined

instance Show Clock where
    show = toString

clockHour :: Clock -> Int
clockHour (Clock hours minutes) = hours 

clockMin :: Clock -> Int
clockMin (Clock hours minutes) = minutes

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock h m
    where
        h  = (hour + dh) `mod` 24
        m  = min `mod` 60
        dh = min `div` 60 

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock h total) = 
        fromHourMin (hour + h) (min + total) 

toString :: Clock -> String
toString clock = 
    padTime (clockHour clock) ++ ":" ++ padTime (clockMin clock)
        where
            padTime x 
                | x < 10    = '0': show x
                | otherwise = show x

-- updated hastily, to conform to new specification