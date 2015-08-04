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
add grade name school
  | school == School.empty = mySchool grade name
  | otherwise = M.insert grade (S.insert name names) school
  where names = if grade == grade'
                then snd . head $ M.toList school
                else S.empty
        grade' = fst .head $ M.toList school



grade :: Int -> School -> [String]
grade g school = case M.lookup g school of
                  Just x -> S.toList x
                  _ -> [""]


sorted :: School -> School
sorted school = school

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


