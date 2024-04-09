module Cipher (caesarDecode, 
               caesarEncode, 
               caesarEncodeRandom) where

import System.Random

shift :: (Int, Char) -> Char
shift = uncurry (((toEnum . (fromEnum 'a' +)) .) . 
        flip flip 26 . (mod .) . 
        flip flip (fromEnum 'a') . ((-) .) . 
        (. fromEnum) . (+))

caesar :: (Int -> Int -> Int) -> String -> String -> String
caesar = ((zipWith (curry shift) . cycle) .) . 
         map . (. fromEnum) . ($ fromEnum 'a')

caesarEncode :: String -> String -> String
caesarEncode = caesar subtract

caesarDecode :: String -> String -> String
caesarDecode = caesar (-)

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = 
    sequence [randomRIO('a', 'z') | _ <- text] 
    >>= \ key -> let cipherText = caesarEncode key text
                 in  return (key, cipherText)
