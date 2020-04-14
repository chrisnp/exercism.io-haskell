module Pangram (isPangram) where

import Data.Char ( toLower )
import Data.List ( nub, sort )

isPangram :: String -> Bool
isPangram = 
  let 
    gram :: String -> String
    gram = filt . nub . sort . map toLower
      where 
        filt = filter (`elem` ['a'..'z'])
  in 
    flip ((==) . gram) ['a'..'z']