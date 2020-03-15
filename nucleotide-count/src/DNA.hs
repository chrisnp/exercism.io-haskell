module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromListWith)
import Data.List (group)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)
type NucMap = Map Nucleotide Int

nucleotideCounts :: String -> Either String NucMap
nucleotideCounts xs 
        | any (invalid) xs = 
            Left  $ "invalid nucleotides"
        | otherwise = 
            Right $ fromListWith (+) $ empty ++ counts xs
        where
            counts = 
                map (\x -> (head x, length x)) 
                . group 
                . map nucl
            invalid x = 
                not $ x `elem` "ACGT" 
            empty = 
                [(A, 0), (C, 0), (G, 0), (T, 0)]
            nucl n
                | n == 'A'  = A
                | n == 'C'  = C 
                | n == 'G'  = G 
                | n == 'T'  = T
                | otherwise = error "this is not DNA"