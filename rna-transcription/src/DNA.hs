module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA [] = Nothing
toRNA xs = traverse dnaToRna xs
    where
        dnaToRna 'G' = Just 'C'
        dnaToRna 'C' = Just 'G'
        dnaToRna 'T' = Just 'A'
        dnaToRna 'A' = Just 'U'
        dnaToRna  _  = Nothing

