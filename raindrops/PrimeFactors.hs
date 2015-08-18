module PrimeFactors (primeFactors) where

isFactor :: Int -> Int -> Bool
isFactor num target = target `mod` num == 0

factorize :: Int -> [Int]
factorize num
  | num == 1 = []
factorize num = factor 2
  where factor divisor
          | divisor `isFactor` num = [divisor, num `div` divisor]
          | divisor == max' = [num]
          | otherwise = factor $ divisor + 1
        max' = ceiling (sqrt $ fromIntegral num :: Double)

primeFactors :: Int -> [Int]
primeFactors num = case factorize num of
                    (x:num':_) -> x : primeFactors num'
                    (x:_) -> [x]
                    [] -> []
