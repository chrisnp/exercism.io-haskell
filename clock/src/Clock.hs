module Clock (addDelta, fromHourMin, toString) where

instance Num Clock where
    (Clock h1 m1) + (Clock h2 m2) = 
            fromHourMin ( h1 + h2 ) ( m1 + m2 )
    (Clock h1 m1) - (Clock h2 m2) = 
            fromHourMin ( h1 - h2 ) ( m1 - m2 )
    fromInteger = 
            fromHourMin 0 . fromIntegral 
    _ * _    = undefined
    abs _    = undefined
    signum _ = undefined

instance Show Clock where
    show = toString

data Clock = Clock { h :: Int, m :: Int } deriving Eq

clockHour :: Clock -> Int
clockHour Clock { h = hours, m = _ } = hours 

clockMin :: Clock -> Int
clockMin Clock { h = _, m = minutes } = minutes

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = 
    let
        hour' = (hour + dh) `mod` 24
        min'  = minute `mod` 60
        dh    = minute `div` 60
    in 
        Clock { h = hour', m = min' } 

toString :: Clock -> String
toString clock = 
    let
        padTime x = if x < 10 then '0': show x else show x
    in
        padTime (clockHour clock) ++ ":" ++ padTime (clockMin clock)

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minute clock =
    let 
        hs = (clockHour clock) + hour
        ms = (clockMin clock) + minute
    in
        fromHourMin hs ms