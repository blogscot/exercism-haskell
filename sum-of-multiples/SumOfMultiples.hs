module SumOfMultiples (sumOfMultiples, sumOfMultiplesDefault) where

import Data.List
import qualified Data.Set as S

-- More efficient implementation of num
-- see http://buffered.io/posts/a-better-nub/
nub' :: (Ord a) => [a] -> [a]
nub' = go S.empty
  where go _ [] = []
        go s (x:xs) | S.member x s = go s xs
                    | otherwise    = x : go (S.insert x s) xs

-- variation on dobbs' implementation
sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples lst limit = sum $ nub' $ concatMap multiples lst
  where multiples n = [n, n * 2.. pred limit]


-- Ben-Baert's List comprehension version
sumOfMultiples' :: [Int] -> Int -> Int
sumOfMultiples' lst limit = sum [ x | x <- [1..pred limit], any((==0) . mod x) lst]


sumOfMultiplesDefault :: Int -> Int
sumOfMultiplesDefault = sumOfMultiples [3, 5]
