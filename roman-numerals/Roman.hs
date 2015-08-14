module Roman (numerals) where

roman :: [(Int, String)]
roman =
  [ (1, "I"),  (5, "V"), (10, "X"), (50, "L"),
    (100, "C"), (100, "X"), (500, "D"), (1000, "M") ]

build :: Int -> Int -> String
build r num =
  case lookup num roman of
    Just s -> concat $ replicate r s
    _ -> error "Oops"

parseThousands :: Int -> String
parseThousands s = build s 1000

parseHundreds :: Int -> String
parseHundreds s
  | s==9 = "CM"
  | s >= 5 = "D" ++ parseHundreds (s - 5)
  | s==4 = "CD"
  | otherwise = build s 100

parseTens :: Int -> String
parseTens s
  | s==9 = "XC"
  | s >= 5 = "L" ++ parseTens (s - 5)
  | s==4 = "XL"
  | otherwise = build s 10

parseOnes :: Int -> String
parseOnes s
  | s==9 = "IX"
  | s >= 5 = "V" ++ parseOnes (s - 5)
  | s==4 = "IV"
  | otherwise = build s 1

pad :: String -> String
pad s = concat (replicate (4 - length s) "0") ++ s

numerals :: Int -> String
numerals num = f . pad $ show num
  where f :: String -> String
        f (w:x:y:z) = parseThousands (read [w]) ++
                      parseHundreds (read [x]) ++
                      parseTens (read [y]) ++
                      parseOnes (read z)
