module SecretHandshake (handshake) where

import Data.Bits (testBit)

handshake :: Int -> [String]
handshake n = reverseWhen (testBit 4) $ map word $ filter (testBit n) [0..3]
        where 
            reverseWhen 
                | True x    = reverse x
                | otherwise = x
            
