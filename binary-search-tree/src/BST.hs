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

import Data.Foldable ( foldl', toList )


data BST a = Empty | BST (BST a) a (BST a) deriving (Eq, Show)

instance Foldable BST where
    foldMap f Empty = mempty
    foldMap f (BST left val right) = foldMap f left `mappend` f val `mappend` foldMap f right

    
bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (BST left _ _)  = Just left

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (BST _ _ right)  = Just right

bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (BST _ val _)  = Just val

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList xs = foldl' (flip insert) empty xs

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (BST left val right) 
        | x <= val  = BST (insert x left) val right
        | otherwise = BST left val (insert x right)

singleton :: a -> BST a
singleton x = BST Empty x Empty

-- We do not need this any more, as the toList from Foldable is used 
-- after defining the Foldable Monoid instance above
-- toList :: BST a -> [a]
-- toList Empty                = []
-- toList (BST left val right) = (toList left) ++ [val] ++ (toList right)