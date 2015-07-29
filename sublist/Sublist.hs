module Sublist(Sublist(..), sublist) where

import Data.List

data Sublist = Sublist | Superlist | Equal | Unequal
  deriving (Eq, Show)

sublist :: Eq a => [a] -> [a] -> Sublist
sublist f s
  | f == s = Equal
  | f `isInfixOf` s = Sublist
  | s `isInfixOf` f = Superlist
  | otherwise = Unequal
