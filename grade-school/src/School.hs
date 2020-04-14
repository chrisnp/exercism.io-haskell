module School (School, 
               add, 
               empty, 
               grade, 
               sorted) where

import Data.Maybe (fromMaybe)
import Data.List (sortBy, sort)
   
type School = [(Int, [String])]


empty :: School
empty = []


add :: Int -> String -> School -> School
add gradeNum student school = 
    case find gradeNum school of 
        Nothing -> 
            (gradeNum, [student]) : school
        Just students -> 
            (gradeNum, student:students) : remove gradeNum school


remove :: Int -> School -> School
remove _ [] = 
    empty
remove y (x:xs) 
    | y == fst x = 
        xs
    | otherwise  = 
        x : remove y xs


find :: Int -> School -> Maybe [String]
find _ [] = 
    Nothing
find x ((y1, y2):ys) 
    | x == y1 = 
        Just $ sort y2
    | otherwise = 
        find x ys  


grade :: Int -> School -> [String]
grade gradeNum school = 
    fromMaybe [] (find gradeNum school)  


sorted :: School -> [(Int, [String])]
sorted = 
    sortBy (\ x y -> compare (fst x) (fst y)) . students
    where students [] = 
                []
          students ((x1, x2):xs) = 
                (x1, sort x2) : students xs