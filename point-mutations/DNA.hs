module DNA (hammingDistance) where

hammingDistance :: String -> String -> Int
hammingDistance s1 s2 = sum' $ zipWith (==) s1 s2
  where sum' = length . filter not

