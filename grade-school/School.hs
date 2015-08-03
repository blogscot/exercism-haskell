module School (School(), add, School.empty, grade, sorted) where

import Data.Map.Strict as M
import Data.Set as S

type Grade = Int
type Student = String
type School = Map Grade (Set Student)

empty :: School
empty = M.singleton 0 S.empty

set1 :: S.Set Student
set1 = S.singleton "Aimee"

mySchool :: Int -> String -> School
mySchool d name = M.singleton d (S.singleton name)

add :: Int -> String -> School -> School
add grade name school = M.singleton grade (S.insert name names)
  where names = snd . head $ M.toList school

grade :: Int -> School -> [String]
grade d school = [""]

sorted :: School -> School
sorted school = school


main = do
    print $ mySchool 2 "Aimee"
    print $ add 2 "Aimee" School.empty
    print $ add 2 "Aimee" $ mySchool 2 "Bradley"
    print $ add 2 "Aimee" $ mySchool 3 "Bradley"


