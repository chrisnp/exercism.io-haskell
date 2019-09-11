module Proverb(recite) where


pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

premise :: String -> String -> String
premise = printf "For want of a %s the %s was lost."

conclusion :: String -> String
conclusion = printf "And all for the want of a %s."

recite :: [String] -> String
recite [] = ""
recite [x] = conclusion x
recite (x:xs) = 
