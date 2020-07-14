module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random

shift :: (Int, Char) -> Char
shift (k, v) = 
    toEnum $ (fromEnum 'a') + mod (fromEnum v + k - (fromEnum 'a')) 26

caesarDecode :: String -> String -> String
caesarDecode = 
    (map shift .) . zip . cycle . map ((-) (fromEnum 'a') . fromEnum)

caesarEncode :: String -> String -> String
caesarEncode = 
    (map shift .) . zip . cycle . map (subtract (fromEnum 'a') . fromEnum)

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    key <- sequence [randomRIO('a', 'z') | _ <- text]
    let cipherText = caesarEncode key text
    return (key, cipherText)
