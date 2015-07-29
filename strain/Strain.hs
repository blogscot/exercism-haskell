module Strain (keep, discard) where

keep :: (a -> Bool) -> [a] -> [a]
keep p lst = [ x | x <- lst, p x]

discard :: (a -> Bool) -> [a] -> [a]
discard p = keep (not . p)
