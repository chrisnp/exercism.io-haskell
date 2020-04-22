module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = 
    let 
        rnaComplement 'G' = Right 'C'
        rnaComplement 'C' = Right 'G'
        rnaComplement 'T' = Right 'A'
        rnaComplement 'A' = Right 'U'
        rnaComplement  n  = Left n
    in
        traverse rnaComplement