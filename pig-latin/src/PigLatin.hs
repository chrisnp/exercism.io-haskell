module PigLatin (translate) where

import Text.Regex.Posix ((=~))

translate :: String -> String
translate xs = (unwords . map pigLatin . words) xs

pigLatin :: [Char] -> String
pigLatin word@(x:xs)  
    | elem x "aeiou"       = word ++ "ay"
pigLatin word@('y':'t':xs) = word ++ "ay"
pigLatin word@('x':'r':xs) = word ++ "ay"
pigLatin word@('q':'u':xs) = xs ++ "qu" ++ "ay"
pigLatin (x:'q':'u':xs)    = xs ++ [x] ++ "qu" ++ "ay"
pigLatin ('c':'h':xs)      = xs ++ "ch" ++ "ay"
pigLatin ('t':'h':'r':xs)  = xs ++ "thr" ++ "ay"
pigLatin ('t':'h':xs)      = xs ++ "th" ++ "ay"
pigLatin ('s':'c':'h':xs)  = xs ++ "sch" ++ "ay"
pigLatin ('r':'h':xs)      = xs ++ "rh" ++ "ay"
pigLatin (x:xs)            = xs ++ [x] ++ "ay"

-- translateWord :: String -> String
-- translateWord word = back ++ front ++ "ay" 
--     where 
--         (front, back) = splt word
        
-- splt :: String -> (String, String)
-- splt word@(x:xs)
--     | elem x []        = ("", "") 
--     | elem x "aeiou"   = ([], word)
--     | word == ('y':'t': xs)    = ([], word)
--     | word == ('x':'r': xs)    = ([], word)
--     | word == ('q':'u': xs)    = ("qu" ++ front, back)
--     | word == (x:'q':'u':xs)   = (front ++ "qu", back)
--     | word == ('c':'h': xs)    = ("ch" ++ front, back)
--     | word == ('t':'h':'r':xs) = ("thr" ++ front, back)
--     | word == ('t':'h': xs)    = ("th" ++ front, back) 
--     | word == ('s':'c':'h':xs) = ("sch" ++ front, back)
--     | otherwise        = (x:front, back) 
--     where 
--         (front, back) = splt xs

    