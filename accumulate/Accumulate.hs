module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate p lst = [p x | x <- lst]

accumulate' :: (a -> b) -> [a] -> [b]
accumulate' _ [] = []
accumulate' p (x:xs) = p x : accumulate' p xs
