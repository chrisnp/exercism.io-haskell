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

allergies :: Int -> [Allergen]
allergies score = 
    filter (flip isAllergicTo score) 
            [Eggs, 
             Peanuts,
             Shellfish,
             Strawberries,
             Tomatoes,
             Chocolate,
             Pollen,
             Cats]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = 
    testBit score (fromEnum allergen)