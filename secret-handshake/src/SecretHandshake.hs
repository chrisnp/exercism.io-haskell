module SecretHandshake (handshake) where

import Data.Bits (bit, (.&.))

actions :: [String]
actions = 
    [
     "wink", 
     "double blink", 
     "close your eyes", 
     "jump"
    ]

handshake :: Int -> [String]
handshake n 
    | bit 4 .&. n == 0 = 
        events
    | otherwise = 
        reverse events
    where
        events = 
            [actions !! i | i <- [0..3], 
                            bit i .&. n /= 0]