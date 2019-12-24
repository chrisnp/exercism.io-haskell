module Isogram (isIsogram) where

import Data.List (nub)
import Data.Char (isAlpha, toLower)

isIsogram :: String -> Bool
isIsogram str =
    let
        normal = map toLower . filter isAlpha $ str
    in 
        normal == nub normal