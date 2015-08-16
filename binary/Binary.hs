module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal str = if isBinary str
                  then sum [2^y | (x,y) <- zip (reverse str) [0..length str], x=='1']
                  else 0
  where isBinary = all (\c -> c =='0' || c=='1')
