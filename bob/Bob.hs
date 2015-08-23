module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor str
  | hasLetters str && isUpperCase str = "Whoa, chill out!"
  | all isSpace str                   = "Fine. Be that way!"
  | last str == '?'                   = "Sure."
  | otherwise                         = "Whatever."
    where
      hasLetters = any isAlpha
      isUpperCase = all isUpper . filter isAlpha