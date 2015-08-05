{-# LANGUAGE BangPatterns #-}

module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f !acc (x:xs) = foldl' f (acc `f` x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc = fun
  where fun [] = acc
        fun (x:xs) = x `f` fun xs

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(x:xs) ++ ys = x : xs ++ ys

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs
