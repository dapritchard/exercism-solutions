module School (School, add, empty, grade, sorted) where

import qualified Data.Map as Map
import Data.Sort

type School = Map.Map Int [String]

-- Add a student to the specified grade in the school
add :: Int -> String -> School -> School
add gradeNum student school =
  case Map.lookup gradeNum school of
    Just x -> Map.insert gradeNum (sort ([student] ++ x)) school
    Nothing -> Map.insert gradeNum [student] school

-- Create an empty school
empty :: School
empty = Map.empty

-- Obtain the students for the specified grade in the school
grade :: Int -> School -> [String]
grade gradeNum school =
  case Map.lookup gradeNum school of
    Just x -> x
    Nothing -> []

-- Obtain a representation of the entire school
sorted :: School -> [(Int, [String])]
sorted school = Map.toList school
