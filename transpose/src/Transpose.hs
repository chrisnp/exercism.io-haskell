module Transpose (transpose) where

import qualified Data.List as DL


transpose :: [String] -> [String]
transpose = 
    let 
        padded = 
            scanr (\xs ys -> 
                    if length ys <= length xs then 
                        xs 
                    else 
                        xs ++ 
                        (replicate (length ys - length xs)
                                   ' ')) 
                  []
    in 
        DL.transpose . padded