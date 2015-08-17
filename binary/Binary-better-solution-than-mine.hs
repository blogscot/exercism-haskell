module Binary (toDecimal) where

import Data.Maybe
import Control.Monad
import Debug.Trace

toDecimal :: String -> Int
toDecimal = fromMaybe 0 . foldM accumulate 0
  where
    accumulate total b = do
      digit <- bit b
      return $ total * 2 + digit
    bit '1' = Just 1
    bit '0' = Just 0
    bit _ = Nothing

-- Just playing around with the above example so I fully understand how it works.

numBits :: String -> Maybe Int
numBits = foldM parse 0
  where
    parse acc b = do
      digit <- bit b
      traceShowM (acc, digit)  -- let's have a peek at what's going on
      return $ acc + digit
    bit '1' = Just 1
    bit '0' = Just 0
    bit _ = Nothing

main = print $ fromMaybe 0 $ numBits "101010101"