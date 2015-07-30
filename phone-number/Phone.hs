module Phone (areaCode, number, prettyPrint) where

import Data.Char

filter' :: String -> String
filter' = filter isDigit

number :: String -> String
number str
  | len == 10 = digits
  | len == 11 && head digits == '1' = tail digits
  | otherwise = "0000000000"
  where digits = filter' str
        len = length digits

areaCode :: String -> String
areaCode = take 3

prettyPrint :: String -> String
prettyPrint num = "(" ++ code ++ ") " ++ middle ++ "-" ++ end
  where code = areaCode num'
        (middle, end) = splitAt 3 $ drop 3 num'
        num' = number num
