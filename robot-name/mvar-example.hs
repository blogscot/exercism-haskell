import Control.Concurrent

newVar :: String -> IO (MVar String)
newVar = newMVar

swapVar :: MVar String -> String -> IO String
swapVar = swapMVar

readVar :: MVar String -> IO String
readVar = readMVar

main = do
  hits <- newVar "Hello"
  _ <- swapVar hits "Hey"  -- unloads the old value
  i <- readVar hits        -- read the swapped value
  print i
