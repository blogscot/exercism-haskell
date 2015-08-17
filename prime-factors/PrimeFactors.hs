module PrimeFactors (primeFactors) where

isFactor :: Int -> Int -> Bool
isFactor num target
  | target `mod` num == 0 = True
  | otherwise = False

factorize :: Int -> [Int]
factorize num
  | num == 1 = []
  | num < 4 = [num]
factorize num = factor 2
  where factor divisor
          | divisor == num = [num]
          | divisor `isFactor` num = [divisor, num `div` divisor]
          | otherwise = factor $ divisor + 1

primeFactors :: Int -> [Int]
primeFactors num = case factorize num of
                    (x:num':_) -> x : primeFactors num'
                    (x:_) -> [x]
                    [] -> []
