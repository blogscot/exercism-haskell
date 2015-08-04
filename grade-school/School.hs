module School (School, add, School.empty, grade, sorted) where

import Data.Map.Strict as M
import Data.List

type Grade = Int
type Student = String
type School = Map Grade [Student]

empty :: School
empty = M.empty

mySchool :: Grade -> Student -> School
mySchool g name = M.singleton g [name]

add :: Grade -> Student -> School -> School
add g name = M.insertWith (++) g [name]

grade :: Grade -> School -> [Student]
grade = M.findWithDefault []

sorted :: School -> [(Grade, [Student])]
sorted school = [(grade', sort names) | (grade', names) <- M.toList school]

school1 :: School
school1 = add 2 "Andy" $ add 7 "Logan" $ add 2 "Blair" $ add 2 "Aimee" $ mySchool 3 "Bradley"

main = do
    print $ mySchool 2 "Aimee"
    print $ add 2 "Aimee" School.empty
    print $ add 2 "Aimee" $ mySchool 2 "Blair"
    print $ mySchool 3 "Bradley"
    print $ add 2 "Aimee" $ mySchool 3 "Bradley"
    print $ grade 2 school1
    print $ grade 3 school1
    print $ grade 4 school1
    print $ sorted school1
    print $ toList school1
