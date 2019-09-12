module Proverb(recite) where

import Text.Printf
import Data.List

-- pairs :: [a] -> [(a, a)]
-- pairs [] = []
-- pairs xs = zip xs (tail xs)

premise :: String -> String -> String
premise (x, y) = printf "For want of a %s the %s was lost." x y

conclusion :: String -> String
conclusion x = printf "And all for the want of a %s." x

recite :: [String] -> String
recite [] = ""
recite xs = verses ++ epilogue
    where
        verses = unlines . map premise $ (zip xs (tail xs))
        epilogue = conclusion $ head xs