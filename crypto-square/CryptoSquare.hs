module CryptoSquare (normalizePlaintext
                     , squareSize
                     , plaintextSegments
                     , ciphertext
                     , normalizeCiphertext) where

import Data.Char (isDigit, isAlpha, toLower)
import Data.List (transpose)

normalizePlaintext :: String -> String
normalizePlaintext = map toLower . filter (\c -> isDigit c || isAlpha c)

squareSize :: String -> Int
squareSize = ceiling . (sqrt :: Double -> Double) . fromIntegral . length

plaintextSegments :: String -> [String]
plaintextSegments str = words $ split len str'
  where str' = normalizePlaintext str
        len = squareSize str'

ciphertext :: String -> String
ciphertext = concat . transpose . plaintextSegments

normalizeCiphertext :: String -> String
normalizeCiphertext = unwords . transpose . plaintextSegments

split :: Int -> String -> String
split num str
  | length str <= num = str
  | otherwise = take num str ++ " " ++ split num (drop num str)
