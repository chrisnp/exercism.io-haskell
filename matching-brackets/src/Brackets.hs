module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = paired xs ""

paired :: [Char] -> [Char] -> Bool
paired "" "" = True
paired "" _  = False
paired (']':xs) ('[':ys) = paired xs ys
paired (')':xs) ('(':ys) = paired xs ys
paired ('}':xs) ('{':ys) = paired xs ys
paired (x:xs) stack
    | x `elem` "]})" = False
    | x `elem` "[{(" = paired xs (x:stack)
    | otherwise      = paired xs stack