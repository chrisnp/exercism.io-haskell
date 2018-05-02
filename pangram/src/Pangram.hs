module Pangram (isPangram) where

import Data.Char (toLower, isLetter)
import Data.List

isPangram :: String -> Bool
isPangram text = length ( group . sort . map toLower . filter isLetter $ text ) == 26
