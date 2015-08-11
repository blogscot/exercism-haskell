module Scrabble (scoreLetter, scoreWord) where

import Data.Map.Strict as M hiding (foldr, map)
import Data.Char

type Score = Int
type Letter = String
type Scrabble = Map Letter Score

scoreLetter :: Char -> Int
scoreLetter c = value
  where Just value = M.lookup [toUpper c] $ fromList scrabble

scoreWord :: String -> Int
scoreWord word = sum $ map scoreLetter word

scrabble :: [(Letter, Score)]
scrabble =
  [ ("A", 1), ("B", 3), ("C", 3), ("D", 2), ("E", 1)
  , ("F", 4), ("G", 2), ("H", 4), ("I", 1), ("J", 8)
  , ("K", 5), ("L", 1), ("M", 3), ("N", 1), ("O", 1)
  , ("P", 3), ("Q", 10), ("R", 1), ("S", 1), ("T", 1)
  , ("U", 1), ("V", 4), ("W", 4), ("X", 8), ("Y", 4)
  , ("Z", 10) ]
