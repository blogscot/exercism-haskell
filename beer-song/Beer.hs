module Beer (sing, verse) where

verse :: Int -> String
verse n
  | n > 0 = howManyBottles n " bottle" ++ " of beer on the wall, " ++
            howManyBottles n " bottle" ++ " of beer.\n" ++
            takeBottles n "Take one" ++ " down and pass it around, " ++
            howManyBottles (pred n) " bottle" ++ " of beer on the wall.\n"
  | otherwise = "No more bottles of beer on the wall, no more bottles of beer.\n" ++
                "Go to the store and buy some more, 99 bottles of beer on the wall.\n"

takeBottles :: Int -> String -> String
takeBottles n s
  | n > 1 = s
  | n == 1 = "Take it"
  | otherwise = error "Negative number of bottles!"

howManyBottles :: Int -> String -> String
howManyBottles n s
  | n > 1 = show n ++ s ++ "s"
  | n == 1 = show n ++ s
  | n == 0 = "no more bottles"
  | otherwise = error "Negative number of bottles!"

sing :: Int -> Int -> String
sing start end = unlines $ map verse [start, pred start..end]
