module Transpose (transpose) where

import qualified Data.List as DL


transpose :: [String] -> [String]
transpose = 
    let 
        padded = 
            scanr (\xs ys -> 
                    let leny = length ys
                        lenx = length xs
                    in
                        if leny <= lenx then xs 
                        else xs ++ (replicate (leny - lenx) ' ')
                  ) []
    in 
        DL.transpose . padded