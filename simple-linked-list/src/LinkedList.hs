module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Nil | LinkedList { datum :: a, next :: LinkedList a } deriving (Eq, Show)


fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x:xs) = LinkedList x (fromList xs)
            

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

new :: a -> LinkedList a -> LinkedList a
new x xs = LinkedList x xs

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = fromList . reverse . toList 

toList :: LinkedList a -> [a]
toList Nil = []
toList (LinkedList x xs) = x : toList xs
     
