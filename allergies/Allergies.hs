module Allergies (Allergen(..), isAllergicTo, allergies) where

import Data.Bits hiding (bit)
import Data.Map.Strict
import Data.Maybe
import Prelude hiding (lookup)

data Allergen = Eggs | Peanuts | Shellfish | Strawberries | Tomatoes |
                Chocolate | Pollen | Cats
  deriving (Eq, Ord, Show, Enum, Bounded)

allergies :: Int -> [Allergen]
allergies n
  | byte == 0 = []
  | otherwise = allergies' byte 1
    where byte = n .&. 255

allergies' :: Int -> Int -> [Allergen]
allergies' byte bit
  | bit > 128 = []
  | byte .&. bit > 0 = toEnum' bit ++ nextBit
  | otherwise = nextBit
    where nextBit = allergies' byte (shiftL bit 1)
          toEnum' b = case lookup b $ fromList scores of
                      Just x -> [x]
                      _ -> []

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo a n = (2^fromEnum a :: Int) .&. n > 0

scores :: [(Int, Allergen)]
scores = [(2^x, toEnum x :: Allergen) | x <- [(fromEnum Eggs)..fromEnum Cats]]
