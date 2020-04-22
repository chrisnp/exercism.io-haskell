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


import Control.Monad (liftM2)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V


data Matrix a = Matrix { 
                vect  :: Vector a,
                rows :: Int,
                cols :: Int
                } deriving (Show, Eq) 

row :: Int -> Matrix a -> Vector a
row x matrix = 
    V.slice ((*) (cols matrix) (x - 1)) 
            (cols matrix) 
            (vect matrix)

column :: Int -> Matrix a -> Vector a
column x matrix = 
    let 
        vs = vect matrix
        cs = cols matrix
    in
        V.fromList 
            (map (vs !) 
            [x - 1, x + cs - 1 .. V.length vs - 1])

flatten :: Matrix a -> Vector a
flatten = 
        vect

fromList :: [[a]] -> Matrix a
fromList []  = 
    Matrix V.empty 0 0
fromList xs = 
    Matrix { vect = V.fromList (concat xs), 
             rows = length xs, 
             cols = length (head xs) }

fromString :: Read a => String -> Matrix a
fromString = 
    fromList . map (map read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape dimensions matrix = 
    Matrix { rows = fst dimensions, 
             cols = snd dimensions, 
             vect = vect matrix }

shape :: Matrix a -> (Int, Int)
shape = 
    liftM2 (,) rows cols

transpose :: Matrix a -> Matrix a
transpose matrix = 
    Matrix { vect = V.concat (map (flip column matrix) 
                                  [1 .. (cols matrix)]),
             rows = cols matrix,
             cols = rows matrix }