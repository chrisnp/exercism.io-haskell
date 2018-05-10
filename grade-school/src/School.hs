module School (School, add, empty, grade, sorted) where

import Data.List (filter, unzip, sort)

type Student = (Int, [String])
type School = [Student]


add :: Int -> String -> School -> School
add gradeNum student school 
    | grade gradeNum school /= empty = [ (gradeNum, sort (student : (grade gradeNum school))) ++ filter (\(x, y) -> x /= gradeNum) school
    | otherwise                      = [ (gradNum, [student]) ] ++ school


empty :: School
empty = []


grade :: Int -> School -> [String]
grade gradeNum school 
    | filter (\ (x, y) -> x == gradeNum) school /= empty = concat $ snd (unzip (filter (\ (x, y) -> x == gradeNum) school))
    | otherwise                                          = []

sorted :: School -> [(Int, [String])]
sorted school = sort school
