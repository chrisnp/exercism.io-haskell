{-# LANGUAGE DeriveFoldable #-}

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


import Data.Foldable ( Foldable, 
                       foldl' )


data BST a = Empty | BST (BST a) a (BST a) 
             deriving (Eq, Show, Foldable)


bstLeft :: BST a -> Maybe (BST a)
bstLeft tree = 
    let 
        (BST left _ _) = tree
    in 
        Just left


bstRight :: BST a -> Maybe (BST a)
bstRight tree = 
    let 
        (BST _ _ right) = tree
    in 
        Just right


bstValue :: BST a -> Maybe a
bstValue Empty = 
    Nothing
bstValue tree = 
    let
        (BST _ val _) = tree
    in 
        Just val


empty :: BST a
empty = Empty


fromList :: Ord a => [a] -> BST a
fromList xs = 
    foldl' (flip insert) empty xs


insert :: Ord a => a -> BST a -> BST a
insert x Empty =
    singleton x
insert x tree 
    | x <= val  = 
        BST (insert x left) val right
    | otherwise =
        BST left val (insert x right)    
    where 
        (BST left val right) = tree


singleton :: a -> BST a
singleton x = 
    BST Empty x Empty


toList :: BST a -> [a]
toList Empty = 
    []
toList tree = 
    let 
        (BST left val right) = tree
    in 
        (toList left) ++ 
        [val] ++ 
        (toList right)
        
