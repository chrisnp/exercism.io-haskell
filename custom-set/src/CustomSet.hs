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
import qualified Data.List as List
import Data.List ( (\\), sort, nub, elem )

data CustomSet a = CustomSet { elems :: [a] } 
                   deriving (Eq)

instance (Show a, Ord a) => Show (CustomSet a) where 
         show set = "fromList " ++ show (elems set)


delete :: (Eq a) => a -> CustomSet a -> CustomSet a
delete x set = (CustomSet . List.delete x . elems) set

difference :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
difference setA setB = CustomSet $ (toList setA) \\ (toList setB)

empty :: CustomSet a
empty = CustomSet []

fromList :: (Eq a, Ord a) => [a] -> CustomSet a
fromList xs =  (CustomSet . sort . nub) xs

insert :: (Ord a) => a -> CustomSet a -> CustomSet a
insert x set = (fromList . (:) x . elems) set

intersection :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = fromList $ (elems setA) `List.intersect` (elems setB)

isDisjointFrom :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = empty == intersection setA setB 

isSubsetOf :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = setB == union setA setB

member :: (Eq a) => a -> CustomSet a -> Bool
member x set = (elem x . elems) set

null :: (Eq a) => CustomSet a -> Bool
null set = set == empty

size ::(Eq a) => CustomSet a -> Int
size set = ( length . elems ) set 

toList :: (Eq a) => CustomSet a -> [a]
toList set = elems set

union :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union setA setB = fromList (toList (difference setA setB) 
                  ++ toList (difference setB setA) 
                  ++ toList (intersection setA setB))
