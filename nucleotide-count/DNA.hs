module DNA (count, nucleotideCounts) where

import Data.Map.Strict (Map, fromList, fromListWith, insertWith, toList)

dna :: String
dna = "ACGT"

validate :: Char -> Char
validate c
  | c `notElem` dna = error $ "invalid nucleotide " ++ show c
  | otherwise = c

count :: Char -> String -> Int
count c str = length $ filter matches $ map validate str
  where matches x = x == validate c

empty :: Map Char Int
empty = fromList $ dna `zip` repeat 0

nucleotideCounts :: String -> Map Char Int
nucleotideCounts [] = empty
nucleotideCounts str = fromListWith (+) $ concatMap insert str
  where insert c = toList $ insertWith (+) (validate c) 1 empty
