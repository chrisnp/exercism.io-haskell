module School2 (School, add, empty, grade, sorted) where

import Data.List

data School = School { students :: [(Int, String)] } 

add :: Int -> String -> School -> School
add gradeNum student school = School $ students school ++ [(gradeNum, student)]

empty :: School
empty = School []

grade :: Int -> School -> [String]
grade gradeNum school = foldl (\acc x -> acc ++ [snd x]) []
                        $ filter (\x -> fst x == gradeNum) $ students school

grades :: School -> [Int]
grades school = sort . nub $ foldl (\acc x -> acc ++ [fst x]) [] $ students school

sorted :: School -> [(Int, [String])]
sorted school = group (grades school) school
  where group [] _ = []
        group (x:xs) school = [(x, sort $ grade x school)] ++ group xs school