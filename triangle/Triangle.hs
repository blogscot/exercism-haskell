module Triangle (TriangleType(..), triangleType) where

import Control.Applicative

data TriangleType = Scalene | Isosceles | Equilateral | Illogical
  deriving (Show, Eq)

triangleType :: Int -> Int -> Int -> TriangleType
triangleType a b c
  | illogical a b c = Illogical
  | a==b && b==c = Equilateral
  | a==b || a==c || b==c = Isosceles
  | otherwise = Scalene

illogical :: Int -> Int -> Int -> Bool
illogical a b c = or $ (\x y z -> x + y <= z) <$> t <*> t <*> t
  where t = [a,b,c]
