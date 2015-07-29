module Strain (keep, discard) where

keep'' :: (a -> Bool) -> [a] -> [a]
keep'' p lst = [ x | x <- lst, p x]

discard :: (a -> Bool) -> [a] -> [a]
discard p = keep (not . p)

keep' :: (a -> Bool) -> [a] -> [a]
keep' _ [] = []
keep' p (x:xs) | p x = x : keep' p xs
               | otherwise = keep' p xs

keep :: (a->Bool) -> [a] -> [a]
keep p = foldr (\x y -> if p x then x:y else y) []