module Bob (responseFor) where

import Data.Char

isLast :: Eq a => a -> [a] -> Bool
isLast _ [] = False
isLast c str = last str == c

hasLetters :: String -> Bool
hasLetters = any isAlpha

isUpperCase :: String -> Bool
isUpperCase = all isUpper . filter isAlpha

responseFor :: String -> String
responseFor str
  | hasLetters str && isUpperCase str = "Whoa, chill out!"
  | filter (not . isSpace) str == ""   = "Fine. Be that way!"
  | '?' `isLast` str                   = "Sure."
  | otherwise                          = "Whatever."
