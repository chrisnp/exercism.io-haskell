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
ability = (sum . drop 1 . sort) <$> (vectorOf 4 $ choose (1, 6))

character :: Gen Character
character = do
    constn <- ability
    let hitpts = 10 + (modifier constn)
    Character <$> ability 
              <*> ability 
              <*> (pure constn)
              <*> ability 
              <*> ability 
              <*> ability 
              <*> (pure hitpts)
              
            
