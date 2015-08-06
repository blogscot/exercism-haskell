module WordCount (wordCount) where

import Data.Char
import Data.Map.Strict as M hiding (map, filter, foldr)

type Word = String
type Count = Int
type WordList = Map Word Count

add :: Word -> WordList -> WordList
add w = M.insertWith (+) w 1

stripSymbols :: String -> String
stripSymbols = map strip
  where strip c = if isAlphaNum c then c else ' '

wordCount :: String -> WordList
wordCount str = foldr add M.empty entries
  where entries = words . map toLower $ stripSymbols str
