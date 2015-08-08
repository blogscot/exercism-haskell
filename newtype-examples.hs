
newtype Okay = ExactlyOne Int deriving Show
newtype Param a b = Param (Either Int String) deriving Show
newtype Record = Record { getInt :: Int} deriving Show

type Grade = Int
type Student = String
newtype School = School { getStudents :: Map Grade Student}

main = do
  print $ ExactlyOne 10
  print $ Param (Left 10)
  print $ Param (Right "Hey")
  print $ Record 10
  print $ getInt $ Record 10
