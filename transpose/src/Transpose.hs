module Transpose (transpose) where

import qualified Data.List (transpose)

transpose :: [String] -> [String]
transpose lines = let padded = scanr (\xs ys -> if length ys > length xs 
                                                then xs ++ (replicate (length ys - length xs) ' ')
                                                else xs) []
                  in Data.List.transpose $ padded lines