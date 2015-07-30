module Anagram (anagramsFor) where

import Data.Char
import Data.List

toLower' :: String -> String
toLower' = map toLower

anagramsFor :: String -> [String] -> [String]
anagramsFor word lst = filtered unfilteredList
  where filtered = filter matches
        matches x = toLower' x /= toLower' word
        unfilteredList = [x | x <- lst, isAnagram x]
        isAnagram x = groupSort (toLower' x) == groupSort (toLower' word)
        groupSort = concat . sort . group
