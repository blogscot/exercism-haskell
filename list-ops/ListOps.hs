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
length = foldr (\_ y -> y+1) 0

reverse :: [a] -> [a]
reverse = foldr (\x y -> y ++ [x]) []

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x y -> if p x then x:y else y) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat [] = []
concat xs = foldr (++) [] xs
