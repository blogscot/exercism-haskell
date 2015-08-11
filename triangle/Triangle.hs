module Triangle (TriangleType(..), triangleType) where

import Data.List

data TriangleType = Scalene | Isosceles | Equilateral | Illogical
  deriving (Show, Eq)

triangleType :: Int -> Int -> Int -> TriangleType
triangleType a b c
  | illogical a b c = Illogical
  | a==b && b==c = Equilateral
  | a==b || a==c || b==c = Isosceles
  | otherwise = Scalene

illogical :: Int -> Int -> Int -> Bool
illogical a b c = badTriangle
  where badTriangle = x <= 0 || x + y <= z
        [x, y, z] = sort [a,b,c]
