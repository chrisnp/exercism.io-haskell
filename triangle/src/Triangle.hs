module Triangle (TriangleType(..), triangleType) where

import Data.List

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Eq a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c =
    let 
        (x:y:z:_) = sort [a, b, c]
        triangleType | x + y <= z || x <= 0 = Illegal
                     | x == z               = Equilateral
                     | x == y || y == z     = Isosceles
                     | otherwise            = Scalene
    in
        triangleType
        