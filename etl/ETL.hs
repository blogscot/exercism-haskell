module ETL (transform) where

import Data.Char
import Data.Map.Strict as M hiding (map)

type Score = Int
type Letter = String
type OldSystem = Map Score [Letter]
type NewSystem = Map Letter Score

transform :: OldSystem -> NewSystem
transform old = M.fromList [(map toLower letter, value) | (value, letters) <- toList old, letter <- letters]
