module Cipher (caesarDecode, 
               caesarEncode, 
               caesarEncodeRandom) where

import System.Random

shift :: (Int, Char) -> Char
shift (k, v) = 
    toEnum $ (fromEnum 'a') + 
    mod (fromEnum v + k - (fromEnum 'a')) 26

caesar :: (Int -> Int -> Int) -> String -> String -> String
caesar f = (map shift .) . zip . cycle . 
            map (f (fromEnum 'a') . fromEnum)

caesarEncode :: String -> String -> String
caesarEncode = caesar subtract

caesarDecode :: String -> String -> String
caesarDecode = caesar (-)

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = 
    sequence [randomRIO('a', 'z') | _ <- text] >>= \ key -> 
    let cipherText = caesarEncode key text
    in 
        return (key, cipherText)
