module Allergies (Allergen(..), isAllergicTo, allergies) where

import Data.Bits

data Allergen = Eggs | Peanuts | Shellfish | Strawberries | Tomatoes | Chocolate | Pollen | Cats
    deriving (Show, Eq, Enum, Bounded)

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo a as = testBit as $ fromEnum a

allergies :: Int -> [Allergen]
allergies as = [a | a <- allergens, isAllergicTo a as]
    where allergens = enumFrom minBound

main = do
  let a = enumFrom (minBound :: Allergen)  -- Turns the data type into an array
  print $ testBit (7 :: Int) 2 -- is bit 2 set?