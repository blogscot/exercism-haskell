module Raindrops (convert) where

import Data.Set as S hiding (map)
import PrimeFactors

convert :: Int -> String
convert num
  | any (\x -> x==3 || x==5 || x==7) factors = convert' factors
  | otherwise = show num
    where factors = primeFactors num

convert' :: [Int] -> String
convert' nums = f nums'
  where nums' = S.toList $ S.fromList nums
        f (n:ns)
          | n==3 = "Pling" ++ convert' ns
          | n==5 = "Plang" ++ convert' ns
          | n==7 = "Plong"
          | otherwise = convert' ns
        f _ = []
