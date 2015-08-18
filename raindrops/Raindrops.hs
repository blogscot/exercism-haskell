module Raindrops (convert) where

import Data.Set hiding (map)
import PrimeFactors

convert :: Int -> String
convert num
  | any (\x -> x==3 || x==5 || x==7) factors = f factors
  | otherwise = show num
    where factors = toList . fromList $ primeFactors num
          f :: [Int] -> String
          f [] = []
          f (n:ns)
            | n==3 = "Pling" ++ f ns
            | n==5 = "Plang" ++ f ns
            | n==7 = "Plong"
            | otherwise = f ns
