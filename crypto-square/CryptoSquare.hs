module CryptoSquare (normalizePlaintext
                     , squareSize
                     , plaintextSegments
                     , ciphertext
                     , normalizeCiphertext) where

import Data.Char
import Debug.Trace

normalizePlaintext :: String -> String
normalizePlaintext = map toLower . filter (\c -> isDigit c || isAlpha c)

squareSize :: String -> Int
squareSize s = ceiling . sqrt . fromIntegral $ length s

plaintextSegments :: String -> [String]
plaintextSegments str = words $ split len str'
  where str' = normalizePlaintext str
        len = squareSize str'

ciphertext :: String -> String
ciphertext str = [c | z <- [0..pred v], (c, pos) <- zip str' [0..(length str')], pos `mod` v == z]
  where str' = normalizePlaintext str
        v = squareSize str'

normalizeCiphertext :: String -> String
normalizeCiphertext str = ciphertext'
  where str' = normalizePlaintext str
        ciphertext' = split (pred v) $ ciphertext str
        v = squareSize str'

main = do
  -- print $ normalizePlaintext "s#!@$%Plunk"
  -- print $ squareSize "1234"
  -- print $ squareSize "123456789abc"
  -- let s = normalizePlaintext "Never vex thine heart with idle woes."
  -- let l = squareSize s
  -- print $ words $ split l s
  -- print $ plaintextSegments "Never vex thine heart with idle woes."
  -- let plain = plaintextSegments "Time is an illusion. Lunchtime doubly so."
  -- print plain
  -- print $ ciphertext "Time is an illusion. Lunchtime doubly so."
  -- print $ ciphertext "Madness, and then illumination."
  -- print $ normalizeCiphertext "Madness, and then illumination."
  -- print $ normalizeCiphertext "Vampires are people too!"
  print $ ciphertext "If man was meant to stay on the ground god would \
                         \have given us roots"
  print $ normalizeCiphertext "If man was meant to stay on the ground god would \
                         \have given us roots"


split :: Int -> String -> String
split num str
  | length str <= num = str
  | otherwise = take num str ++ " " ++ split num (drop num str)