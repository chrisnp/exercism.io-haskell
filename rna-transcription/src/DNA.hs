module DNA (toRNA) where

toRNA :: String -> Either Char String
-- toRNA [] = Nothing
toRNA xs = traverse rnaComplement xs
    where
        rnaComplement 'G' = Right 'C'
        rnaComplement 'C' = Right 'G'
        rnaComplement 'T' = Right 'A'
        rnaComplement 'A' = Right 'U'
        rnaComplement  n  = Left n