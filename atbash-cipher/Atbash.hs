module Atbash (encode) where

import Data.Char
import Data.Maybe

split :: Int -> String -> String
split num str
  | length str <= num = str
  | otherwise = take num str ++ " " ++ split num (drop num str)

encode :: String -> String
encode str = split 5 $ mapMaybe encipher str
  where encipher c = toLower c `lookup` zip plain cipher

raw, digits, plain, cipher :: String
raw = "abcdefghijklmnopqrstuvwxyz"
digits = "0123456789"
plain = raw ++ digits
cipher = reverse raw ++ digits
