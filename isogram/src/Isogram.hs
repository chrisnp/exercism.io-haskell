module Isogram (isIsogram) where

import Data.List ( nub )
import Data.Char ( isAlpha, toLower )


isIsogram :: String -> Bool
isIsogram = 
    let 
        fix f = let {x = f x} in x
    in
        (<*>) (==) nub . fix . const . map toLower . (filter isAlpha)
