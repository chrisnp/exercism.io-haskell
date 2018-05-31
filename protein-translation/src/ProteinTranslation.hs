module ProteinTranslation(proteins) where

import Data.List

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

codons :: [String]
codons =  ["AUG","UUU","UUC","UUA","UUG","UCU","UCC","UCA","UCG", "UAU","UAC","UGU","UGC","UGG","UAA","UAG","UGA"]

validRnaSeq :: String -> Bool
validRnaSeq seq = length seq `mod` 3 == 0 && all (`elem`codons) (splitEvery 3 seq)

amino :: String -> String
amino c
    | c == "AUG"                            = "Methionine"
    | c == "UGG"                            = "Tryptophan"
    | c `elem` ["UUU", "UUC"]               = "Phenylalanine"
    | c `elem` ["UUA", "UUG"]               = "Leucine"
    | c `elem` ["UAU", "UAC"]               = "Tyrosine"
    | c `elem` ["UGU", "UGC"]               = "Cysteine"
    | c `elem` ["UCU", "UCC", "UCA", "UCG"] = "Serine"
    -- | c `elem` ["UAA", "UAG", "UGA"]        = "STOP"
    | otherwise                             = ""

proteins :: String -> Maybe [String]
proteins rna 
    | validRnaSeq rna = Just (takeWhile (/= "") (map amino (splitEvery 3 rna)))
    | otherwise       = Nothing
 