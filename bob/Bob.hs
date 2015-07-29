module Bob (responseFor) where

import Data.Char

islast :: Eq a => a -> [a] -> Bool
islast _ [] = False
islast c str = last str == c

hasLetters :: String -> Bool
hasLetters = any isAlpha

allUpperCase :: String -> Bool
allUpperCase = all isUpper . filter isAlpha

responseFor :: String -> String
responseFor str
  | ('!' `elem` str || '?' `elem` str) && hasLetters str && allUpperCase str = "Whoa, chill out!"
  | filter (not . isSpace) str == ""                                         = "Fine. Be that way!"
  | '?' `islast` str                                                         = "Sure."
  | '?' `islast` str && filter isAlpha str == ""                             = "Sure."
  | allUpperCase str && (not . any isDigit) str                              = "Whoa, chill out!"
  | otherwise                                                                = "Whatever."
