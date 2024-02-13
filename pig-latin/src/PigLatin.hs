module PigLatin (translate) where

translate :: String -> String
translate xs = (unwords . map pigLatin . words) xs

pigLatin :: [Char] -> String
pigLatin []               = []
pigLatin word@(x:_)  
    |elem x "aeiou"       = word ++ "ay"
pigLatin word@('y':'t':_) = word ++ "ay"
pigLatin word@('x':'r':_) = word ++ "ay"
pigLatin ('q':'u':xs)     = xs ++ "qu" ++ "ay"
pigLatin (x:'q':'u':xs)   = xs ++ [x] ++ "qu" ++ "ay"
pigLatin ('c':'h':xs)     = xs ++ "ch" ++ "ay"
pigLatin ('t':'h':'r':xs) = xs ++ "thr" ++ "ay"
pigLatin ('t':'h':xs)     = xs ++ "th" ++ "ay"
pigLatin ('s':'c':'h':xs) = xs ++ "sch" ++ "ay"
pigLatin ('r':'h':xs)     = xs ++ "rh" ++ "ay"
pigLatin (x:xs)           = xs ++ [x] ++ "ay"
