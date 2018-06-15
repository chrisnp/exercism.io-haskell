module Clock (clockHour, clockMin, fromHourMin, toString) where

data Clock = Clock { hours :: Int, minutes :: Int } deriving Eq

instance Num Clock where
    (+) (Clock h1 m1) (Clock h2 m2) = fromHourMin ( h1 + h2 ) ( m1 + m2 )
    (-) (Clock h1 m1) (Clock h2 m2) = fromHourMin ( h1 - h2 ) ( m1 - m2 )
    fromInteger = fromHourMin 0 . fromIntegral 
    abs _ = undefined
    signum _ = undefined
    (*) _ _ = undefined

instance Show Clock where
    show = toString

clockHour :: Clock -> Int
clockHour clock@(Clock hours minutes) = hours 

clockMin :: Clock -> Int
clockMin clock@(Clock hours minutes) = minutes

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock hour' min'
    where
        hour'  = (hour + deltaH) `mod` 24
        min'   = min `mod` 60
        deltaH = min `div` 60 

toString :: Clock -> String
toString clock = padZero (clockHour clock) ++ ":" ++ padZero (clockMin clock)
        where
            padZero x 
                | x < 10    = '0': show x
                | otherwise = show x
