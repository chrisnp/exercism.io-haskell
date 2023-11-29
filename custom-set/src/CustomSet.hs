module CustomSet 
    ( 
    delete
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
    ) 
    where

import Prelude hiding ( null )
import qualified Data.List as List
import Data.List ( (\\), sort, nub )

data CustomSet a = CustomSet { elements :: [a] } deriving (Eq)

instance (Show a, Ord a) => Show (CustomSet a) where 
    show set = "fromList " ++ show (elements set)


delete :: (Eq a) => a -> CustomSet a -> CustomSet a
delete = (CustomSet .) . (. elements) . List.delete

difference :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
difference = (CustomSet .) . (. toList) . (\\) . toList

empty :: CustomSet a
empty = CustomSet ([])

fromList :: (Eq a, Ord a) => [a] -> CustomSet a
fromList = CustomSet . sort . nub

insert :: (Ord a) => a -> CustomSet a -> CustomSet a
insert = (fromList .) . (. elements) . (:)

intersection :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> CustomSet a
intersection = (fromList .) . (. elements) . List.intersect . elements

isDisjointFrom :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom = ((empty ==) .) . intersection

isSubsetOf :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf = (<*>) (==) . union

member :: (Eq a) => a -> CustomSet a -> Bool
member = (. elements) . elem

null :: (Eq a) => CustomSet a -> Bool
null = (empty ==)

size ::(Eq a) => CustomSet a -> Int
size = length . elements 

toList :: (Eq a) => CustomSet a -> [a]
toList = elements

union :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union setX setY = 
    let 
        y:setYs = toList setY
    in
        if setY == empty then setX else union (insert y setX) (fromList setYs)
