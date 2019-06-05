module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, vectorOf, choose)
import Data.List (sort)

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier = flip div 2 . subtract 10

ability :: Gen Int
ability = fmap (sum . drop 1 . sort) (vectorOf 4 $ choose (1, 6))

character :: Gen Character
character = do
    constitution <- ability 
    Character <$> ability 
              <*> ability 
              <*> (pure constitution)
              <*> ability 
              <*> ability 
              <*> ability 
              <*> (pure $ 10 + (modifier constitution))
              
            
