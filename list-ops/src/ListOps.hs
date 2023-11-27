module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z xs = case xs of 
  []   -> z
  y:ys -> seq z foldl' f ( f z y) ys

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z xs = case xs of
  []   -> z
  y:ys -> f y ( foldr f z ys)

length :: [a] -> Int
length = foldl' (const . (1 +)) 0

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) ([])

map :: (a -> b) -> [a] -> [b]
map = flip foldr ([]) . ((:) .)

filter :: (a -> Bool) -> [a] -> [a]
filter = 
  let 
    if' True  x _ = x
    if' False _ y = y
  in 
    flip foldr ([]) . flip flip id . (<*> (:)) . (if' .)

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) ([])