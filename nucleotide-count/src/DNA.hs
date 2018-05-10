module DNA (nucleotideCounts) where

import Data.Map (Map, fromListWith)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs 
        | any (invalid) xs = Left  $ "invalid nucleotides"
        | otherwise        = Right $ fromListWith (+) $ empty ++ counts xs
        where
            counts = map (\c -> (c, 1))
            invalid x = not $ x `elem` "ACGT" 
            empty = [('A', 0), ('C', 0), ('G', 0), ('T', 0)]

