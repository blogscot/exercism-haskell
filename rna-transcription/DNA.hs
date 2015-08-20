module DNA (toRNA) where

import Data.Maybe

toRNA :: String -> String
toRNA = mapMaybe toRNA'
  where toRNA' c = c `lookup` zip "GCTA" "CGAU"
