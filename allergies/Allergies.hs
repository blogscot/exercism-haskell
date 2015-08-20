module Allergies (Allergen(..), isAllergicTo, allergies) where

import Data.Bits
import Data.Map.Strict
import Data.Maybe
import Prelude hiding (lookup)

data Allergen = None | Eggs | Peanuts | Shellfish | Strawberries | Tomatoes |
                Chocolate | Pollen | Cats
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype Allergy = Allergy {getAllergies :: Map Int Allergen}

allergies :: Int -> [Allergen]
allergies n
  | byte == 0 = []
  | byte .&. 1 > 0 = toEnum' byte ++ allergies (shiftR byte 1)
  | otherwise = allergies (rotateR byte 1)
    where byte = n .&. 255
          toEnum' b = case lookup b $ fromList scores of
                      Just x -> [x]
                      _ -> []

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo a n = (2^pred (fromEnum a) :: Int) .&. n > 0


scores :: [(Int, Allergen)]
scores = [(2^pred x, toEnum x :: Allergen) | x <- [(fromEnum Eggs)..fromEnum Cats]]

main = do
  print scores
  print $ allergies 0
  print $ allergies 1
  print $ allergies 3
  -- print $ (5 :: Int) .&. 2
  -- print $ (2^fromEnum Cats :: Int) .&. 128 > 0
  print $ isAllergicTo Eggs 1
  print $ isAllergicTo Eggs 5
  print $ isAllergicTo Eggs 8
