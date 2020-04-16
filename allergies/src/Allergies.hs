module Allergies (Allergen(..), 
                  allergies, 
                  isAllergicTo) where

import Data.Bits (testBit)

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Enum, Eq, Show)


allergens :: [Allergen]
allergens = [Eggs, 
             Peanuts,
             Shellfish,
             Strawberries,
             Tomatoes,
             Chocolate,
             Pollen,
             Cats]


allergies :: Int -> [Allergen]
allergies = 
    flip filter allergens . flip isAllergicTo


isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo = flip testBit . fromEnum