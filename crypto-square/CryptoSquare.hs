module CryptoSquare (normalizePlaintext
                     , squareSize
                     , plaintextSegments
                     , ciphertext
                     , normalizeCiphertext) where

import Data.Char
import Data.List

normalizePlaintext :: String -> String
normalizePlaintext = map toLower . filter (\c -> isDigit c || isAlpha c)

squareSize :: String -> Int
squareSize s = ceiling (sqrt $ fromIntegral $ length s :: Double)

plaintextSegments :: String -> [String]
plaintextSegments str = words $ split len str'
  where str' = normalizePlaintext str
        len = squareSize str'

ciphertext :: String -> String
ciphertext str = [c | z <- [0..pred v], (c, pos) <- zip str' [0..(length str')], pos `mod` v == z]
  where str' = normalizePlaintext str
        v = squareSize str'

normalizeCiphertext :: String -> String
normalizeCiphertext = unwords . transpose . plaintextSegments

split :: Int -> String -> String
split num str
  | length str <= num = str
  | otherwise = take num str ++ " " ++ split num (drop num str)