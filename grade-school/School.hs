module School (School, add, School.empty, grade, sorted) where

import Data.Map.Strict as M
import Data.List

type Grade = Int
type Student = String
newtype School = School { getStudents :: Map Grade [Student]}

empty :: School
empty = School M.empty

add :: Grade -> Student -> School -> School
add g name = School . M.insertWith (++) g [name] . getStudents

grade :: Grade -> School -> [Student]
grade g = M.findWithDefault [] g . getStudents

sorted :: School -> [(Grade, [Student])]
sorted = M.toAscList . fmap sort . getStudents
