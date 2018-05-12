module Isogram (isIsogram) where

import Data.List (nub)
import Data.Char (isAlpha, toLower)

isIsogram :: String -> Bool
isIsogram str = normal == nub normal
    where
        normal = map toLower . filter isAlpha $ str

