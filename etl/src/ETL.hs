module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)
-- import Data.Maybe


letterScore :: (a, String) -> [(Char, a)]
letterScore (score, letters) = [(toLower letter, score) | letter <- letters ]

transform :: Map a String -> Map Char a
transform legacyData = fromList $ concatMap letterScore $ toList legacyData
