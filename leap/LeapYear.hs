module LeapYear (isLeapYear) where

isLeapYear :: Int -> Bool
isLeapYear year = isDivisibleBy 4 && not (isDivisibleBy 100) || isDivisibleBy 400
  where isDivisibleBy x = year `mod` x == 0

main = do
  print $ isLeapYear 1996
  print $ isLeapYear 1997
  print $ isLeapYear 1900
  print $ isLeapYear 2400
