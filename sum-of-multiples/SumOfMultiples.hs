module SumOfMultiples (sumOfMultiples, sumOfMultiplesDefault) where

import qualified Data.Set as S

-- More efficient implementation of nub
-- see http://buffered.io/posts/a-better-nub/
nub' :: (Ord a) => [a] -> [a]
nub' = go S.empty
  where go _ [] = []
        go s (x:xs) | S.member x s = go s xs
                    | otherwise    = x : go (S.insert x s) xs

-- variation on dobbs' implementation
sumOfMultiples :: Integral a => [a] -> a -> a
sumOfMultiples lst limit = sum . nub' $ concatMap multiples lst
  where multiples n = [n, n * 2.. pred limit]

-- Ben-Baert's List comprehension version
sumOfMultiples' :: Integral a => [a] -> a -> a
sumOfMultiples' lst limit = sum [ x | x <- [1..pred limit], any((==0) . rem x) lst]

sumOfMultiplesDefault :: Int -> Int
sumOfMultiplesDefault = sumOfMultiples [3, 5]
