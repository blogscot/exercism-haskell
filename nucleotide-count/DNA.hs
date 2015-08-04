module DNA (count, nucleotideCounts) where

import Data.Map.Strict (Map, fromList)

dna :: String
dna = "ACGT"

validate :: Char -> Char
validate c
  | c `notElem` dna = error $ "invalid nucleotide " ++ show c
  | otherwise = c

count :: Char -> String -> Int
count c str = length $ filter matches $ map validate str
  where matches x = x == validate c

nucleotideCounts :: String -> Map Char Int
nucleotideCounts str = fromList [(c, count c str) | c <- dna]
