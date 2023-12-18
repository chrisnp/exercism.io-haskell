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


delete :: (Ord a, Eq a) => a -> CustomSet a -> CustomSet a
delete x = fromList . deleteBy (==) x . elements

difference :: (Ord a, Eq a) => CustomSet a -> CustomSet a -> CustomSet a
difference setA setB = fromList $ (toList setA) \\ (toList setB)

empty :: CustomSet a
empty = CustomSet []

fromList :: (Ord a, Eq a) => [a] -> CustomSet a
fromList = CustomSet . sort . nub

insert :: (Ord a) => a -> CustomSet a -> CustomSet a
insert x = fromList . (:) x . elements

intersection :: (Ord a, Eq a) => CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = 
    let 
      bs = toList setB
    in
      fromList $ [b | b <- bs, member b setA]

isDisjointFrom :: (Ord a, Eq a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = empty == intersection setA setB 

isSubsetOf :: (Ord a, Eq a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = setB == union setA setB

member :: (Eq a) => a -> CustomSet a -> Bool
member x = elem x . elements

null :: (Eq a) => CustomSet a -> Bool
null = (empty ==)

size ::(Eq a) => CustomSet a -> Int
size = length . elements 

toList :: (Eq a) => CustomSet a -> [a]
toList = elements

union :: (Ord a, Eq a) => CustomSet a -> CustomSet a -> CustomSet a
union setA setB = 
    let 
      as = toList setA
      bs = toList setB
    in
      fromList $ as ++ [b | b <- bs, not (member b setA)]