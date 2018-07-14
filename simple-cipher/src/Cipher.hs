module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random

shift :: (Int, Char) -> Char
shift (k, v) = toEnum $ (fromEnum 'a') + mod (fromEnum v + k - (fromEnum 'a')) 26

caesarDecode :: String -> String -> String
caesarDecode key encodedText = (map shift . zip (cycle (map ((-) (fromEnum 'a') . fromEnum) key))) encodedText

caesarEncode :: String -> String -> String
caesarEncode key text = (map shift . zip (cycle (map (flip (-) (fromEnum 'a') . fromEnum) key))) text

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    key <- sequence [randomRIO('a', 'z') | _ <- text]
    let cipherText = caesarEncode key text
    return (key, cipherText)
