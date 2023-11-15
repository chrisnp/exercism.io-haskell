module Proverb(recite) where

import Text.Printf

pairs :: [a] -> [(a, a)]
pairs xs = case xs of 
    [] -> []
    ys -> zipWith (,) ys (tail ys)

recite :: [String] -> String
recite xs = 
    let 
        premise :: (String, String) -> String
        premise = uncurry (printf "For want of a %s the %s was lost.\n")
        conclusion :: String -> String
        conclusion = printf "And all for the want of a %s."
    in
        case xs of 
            [] -> []
            ys -> 
                let
                    verses =  map premise (pairs ys)
                    epilogue = conclusion $ head ys
                in 
                    concat $ (++) verses [epilogue]