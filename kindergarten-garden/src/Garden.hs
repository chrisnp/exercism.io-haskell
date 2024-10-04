module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List
import Data.Map ( Map, fromList, (!) )

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Child = String
type Garden = Map Child [Plant]

garden :: [Child] -> String -> Garden
garden students plants = 
    let 
        (split, x:line) = span (/= '\n') plants
        childPlantsMap _ ([], []) = []
        childPlantsMap (c:cs) (a:b:line1, d:e:line2) = 
            (c, [toPlant a, toPlant b, toPlant d, toPlant e]) : 
            childPlantsMap cs (line1, line2)
    in 
        fromList $ childPlantsMap (sort students) (split, line)

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student garden = flip (!) student garden

-- Auxiliary

toPlant :: Char -> Plant
toPlant p =
    let 
        plant | p == 'C' = Clover
              | p == 'G' = Grass
              | p == 'R' = Radishes
              | p == 'V' = Violets
    in 
        plant
