module ProteinTranslation(proteins) where

import Data.List

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

amino :: String -> String
amino a
    | a == "AUG"                            = "Methionine"
    | a == "UGG"                            = "Tryptophan"
    | a `elem` ["UUU", "UUC"]               = "Phenylalanine"
    | a `elem` ["UUA", "UUG"]               = "Leucine"
    | a `elem` ["UAU", "UAC"]               = "Tyrosine"
    | a `elem` ["UGU", "UGC"]               = "Cysteine"
    | a `elem` ["UCU", "UCC", "UCA", "UCG"] = "Serine"
    | a `elem` ["UAA", "UAG", "UGA"]        = "STOP"
    | otherwise                             = ""

proteins :: String -> Maybe [String]
proteins "" = Just []
proteins (n1:n2:n3:xs) = case amino [n1, n2, n3] of
    "STOP" -> Just []
    p      -> case proteins xs of
                  Nothing  -> Nothing
                  Just s   -> Just (p:s)
proteins _ = Nothing


 