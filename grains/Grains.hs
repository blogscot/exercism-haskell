module Grains (square, total) where

-- 1 2 4 8 16
square :: Integer -> Integer
square x = 2 ^ pred x

total :: Integer
total = sum $ map square [1..64]

-- Alternatively, summing a geometric series
-- let    a = 1, r = 2, n = 64
-- in     a (1 - r^n) / (1 - r)
total' :: Integer
total' = 1 * (1-2^64) `div` (1-2)