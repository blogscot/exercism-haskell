module SumOfMultiples (sumOfMultiples, sumOfMultiplesDefault) where

import Data.List

sumOfMultiplesDefault :: Int -> Int
sumOfMultiplesDefault n = sum $ nub [x | x <- [1..pred n], hasFactor x 3 || hasFactor x 5]
  where hasFactor x y = x `mod` y == 0

sumOfMultiples' :: Int -> Int -> [Int]
sumOfMultiples' n fact' = [ x | x <- [1..pred n], hasFactor x fact']
  where hasFactor x y = x `mod` y == 0

sumOfMultiples'' :: [Int] -> Int -> [Int]
sumOfMultiples'' [] _ = [0]
sumOfMultiples'' (x:xs) n = nub $ sumOfMultiples' n x ++ sumOfMultiples'' xs n

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples lst n = sum $ sumOfMultiples'' lst n

main = print $ sumOfMultiples [4, 6] 15


