module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

import Data.List ( foldl )

data BST a = Empty | BST { left  :: Maybe (BST a), 
                           value :: Maybe a,
                           right :: Maybe (BST a)
                         } deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft tree = Just left tree

bstRight :: BST a -> Maybe (BST a)
bstRight tree = Just right tree

bstValue :: BST a -> Maybe a
bstValue tree = Just value tree

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList []     = Empty
fromList (x:xs) = foldl (flip insert) (singleton x) xs

insert :: Ord a => a -> BST a -> BST a
insert x Empty = BST Empty x Empty
insert x tree  
        | x <= (value tree) = BST (insert x (bstLeft tree)) (bstValue tree) (bstRight tree)
        | otherwise         = BST (bstLeft tree) (value tree) (insert x (bstRight tree)) 

singleton :: a -> BST a
singleton x = BST Nothing x Nothing

toList :: BST a -> [Maybe a]
toList tree = 
    let 
        leftList = Maybe [] toList (bstLeft tree)
        rightList = Maybe [] toList (bstRight tree)
    in 
        leftList ++ [(bstValue tree)] ++ rightList