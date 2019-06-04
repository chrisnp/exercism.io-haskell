module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = mapM fromDNA
    where
        fromDNA 'G' = Right 'C'
        fromDNA 'C' = Right 'G'
        fromDNA 'T' = Right 'A'
        fromDNA 'A' = Right 'U'
        fromDNA  x  = Left x