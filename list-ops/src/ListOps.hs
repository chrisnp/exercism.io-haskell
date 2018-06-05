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
foldl' _ z []     = z
foldl' f z (x:xs) = z `seq` foldl' f ( f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x ( foldr f z xs )

length :: [a] -> Int
length xs = foldl' (\z _ -> z + 1) 0 xs

reverse :: [a] -> [a]
reverse xs = foldl' (flip (:)) [] xs

map :: (a -> b) -> [a] -> [b]
map f xs = foldr (\x zs -> f x:zs) [] xs

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = foldr (\x zs -> if p x then x:zs else zs ) [] xs

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss
