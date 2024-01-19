module ProteinTranslation(proteins) where

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = 
    let 
        (as, bs) = splitAt n xs
    in 
        as : splitEvery n bs

codons :: [String]
codons =  [ "AUG"
          , "UGG"
          , "UUU", "UUC"
          , "UUA", "UUG"
          , "UAU", "UAC"
          , "UGU", "UGC"
          , "UCU", "UCC", "UCA", "UCG"
          , "UAA", "UAG" , "UGA"]

validRnaSeq :: String -> Bool
validRnaSeq = 
    ((&&) . (0 ==) . (`mod` 3) . length) <*> 
    (all (`elem` codons) . splitEvery 3)

amino :: String -> String
amino cod
    | cod == "AUG" = "Methionine"
    | cod == "UGG" = "Tryptophan"
    | cod `elem` ["UUU", "UUC"] = "Phenylalanine"
    | cod `elem` ["UUA", "UUG"] = "Leucine"
    | cod `elem` ["UAU", "UAC"] = "Tyrosine"
    | cod `elem` ["UGU", "UGC"] = "Cysteine"
    | cod `elem` ["UCU", "UCC", "UCA", "UCG"] = "Serine"
    -- | cod `elem` ["UAA", "UAG", "UGA"] = "STOP"
    | otherwise = ""

proteins :: String -> Maybe [String]
proteins rna =
    case validRnaSeq rna of
        True -> Just (takeWhile (/= "") (map amino (splitEvery 3 rna)))
        _    -> Nothing