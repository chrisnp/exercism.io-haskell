module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)
import Data.List ( (\\), sort, nub, deleteBy )

data CustomSet a = CustomSet { elements :: [a] } deriving (Eq)

instance (Show a, Ord a) => Show (CustomSet a) 
  where 
    show = ("fromList " ++) . show . elements


fromList :: (Ord a, Eq a) => [a] -> CustomSet a
fromList = CustomSet . sort . nub

toList :: (Eq a) => CustomSet a -> [a]
toList = elements

empty :: CustomSet a
empty = CustomSet []

null :: (Eq a) => CustomSet a -> Bool
null = (empty ==)

delete :: (Ord a, Eq a) => a -> CustomSet a -> CustomSet a
delete x = fromList . deleteBy (==) x . elements

insert :: (Ord a) => a -> CustomSet a -> CustomSet a
insert x = fromList . (:) x . elements

member :: (Eq a) => a -> CustomSet a -> Bool
member x = elem x . elements

size ::(Eq a) => CustomSet a -> Int
size = length . elements 

difference :: (Ord a, Eq a) => CustomSet a -> CustomSet a -> CustomSet a
difference setA setB = fromList $ (toList setA) \\ (toList setB)

isDisjointFrom :: (Ord a, Eq a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = empty == intersection setA setB 

isSubsetOf :: (Ord a, Eq a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = setB == union setA setB

intersection :: (Ord a, Eq a) => CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = 
    let 
      bs = toList setB
    in
      fromList $ [b | b <- bs, member b setA]

union :: (Ord a, Eq a) => CustomSet a -> CustomSet a -> CustomSet a
union setA setB = 
    let 
      as = toList setA
      bs = toList setB
    in
      fromList $ as ++ [b | b <- bs, not (member b setA)]