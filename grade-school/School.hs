module School (School, add, School.empty, grade, sorted) where

import Data.Map.Strict as M
import Data.List

type Grade = Int
type Student = String
type School = Map Grade [Student]

empty :: School
empty = M.empty

add :: Grade -> Student -> School -> School
add g name = M.insertWith (++) g [name]

grade :: Grade -> School -> [Student]
grade = M.findWithDefault []

sorted :: School -> [(Grade, [Student])]
sorted = M.toAscList . fmap sort
