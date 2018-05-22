module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.Map
import Data.List


data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Child = String
type Garden = Map Child [Plant]


toPlant :: Char -> Plant
toPlant p
    | p == 'C' = Clover
    | p == 'G' = Grass
    | p == 'R' = Radishes
    | p == 'V' = Violets


garden :: [Child] -> String -> Garden
garden students plants = fromList $ childPlantsMap (sort students) (splt, line)
        where
            (splt, x:line) = span (/= '\n') plants
            childPlantsMap _ ([], []) = []
            childPlantsMap (c:cs) (a:b:line1, d:e:line2) = 
                    (c, [toPlant a, toPlant b, toPlant d, toPlant e]) : childPlantsMap cs (line1, line2)

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student garden = flip (!) student garden