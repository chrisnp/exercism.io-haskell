module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V

data Matrix a = Matrix { 
                vect :: Vector a,
                rows :: Int,
                cols :: Int
                } deriving (Show, Eq) 

row :: Int -> Matrix a -> Vector a
row x matrix = V.take (cols matrix) $ snd $ V.splitAt (x * (cols matrix)) $ vect matrix
                
-- rows :: Matrix a -> Int
-- rows matrix = rows matrix -- error "You need to implement this function."

-- cols :: Matrix a -> Int
-- cols matrix = error "You need to implement this function."

column :: Int -> Matrix a -> Vector a
column x matrix = V.fromList $ takeNth (x + 1) $ V.toList $ vect matrix
    where 
        takeNth n a = case drop (n - 1) a of 
            (y:ys) -> y : takeNth (cols matrix) ys
            []     -> []      

flatten :: Matrix a -> Vector a
flatten matrix = vect matrix

fromList :: [[a]] -> Matrix a
fromList []  = Matrix V.empty 0 0
fromList xss = Matrix (V.fromList $ concat xss) (length xss) (length $ head xss) 

fromString :: Read a => String -> Matrix a
fromString xs = (fromList . map (map read . words) . lines) xs

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape dimensions matrix = Matrix { rows = fst dimensions, cols = snd dimensions, vect = vect matrix }


shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

transpose :: Matrix a -> Matrix a
transpose matrix = fromList $ [V.toList $ column (c - 1) matrix | c <- [1..(cols matrix)]]
