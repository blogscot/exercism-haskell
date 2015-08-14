module Roman (numerals) where

-- First approach, tried and failed

roman :: [(Int, String)]
roman =
  [ (1, "I"),  (5, "V"), (10, "X"), (50, "L"),
    (100, "C"), (100, "X"), (500, "D"), (1000, "M") ]

divisor :: Int -> Int -> (Int, Int)
divisor d num = num `divMod` d

thousands, fiveHundreds, hundreds, fifties, tens, fives :: Int -> (Int, Int)

thousands    = divisor 1000
fiveHundreds = divisor 500
hundreds     = divisor 100
fifties      = divisor 50
tens         = divisor 10
fives        = divisor 5

parse :: IO ()
parse = do
  let (t, r) = thousands 1888
      (f, r') = fiveHundreds r
  print [t,f,r]

build :: Int -> Int -> String
build r num =
  case lookup num roman of
    Just s -> shorten . concat $ replicate r s
    _ -> error "Oops"

shorten :: String -> String
shorten s
 | s=="IIII" = "IV"
 | s=="XXXX" = "XL"
 | s=="CCCC" = "CD"
 | otherwise = s


numerals :: Int -> String
numerals num =
  case thousands num of
    (x, rest) -> case fiveHundreds rest of
      (x1, rest1) -> case hundreds rest1 of
        (x2, rest2) -> case fifties rest2 of
          (x3, rest3) -> case tens rest3 of
            (x4, rest4) -> case fives rest4 of
              (x5, rest5) ->
                build x 1000 ++ build x1 500 ++ build x2 100 ++ build x3 50 ++ build x4 10 ++ build x5 5 ++ build rest5 1



main =
  print $ numerals 2684
  -- print $ build 2 500

