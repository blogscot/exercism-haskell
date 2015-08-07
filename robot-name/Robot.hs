module Robot (robotName, mkRobot, resetName) where

import System.Random
import Control.Concurrent

mkRobot :: IO (MVar String)
mkRobot = generateName >>= newMVar

resetName :: MVar String -> IO ()
resetName s = do
  _ <- swapMVar s =<< generateName
  return ()

robotName :: MVar String -> IO String
robotName = readMVar

generate :: Int -> (Char, Char) -> IO String
generate n (low, high) = do
  g <- newStdGen
  return $ take n $ randomRs(low, high) g

generateName :: IO String
generateName = do
  start <- generate 2 ('A', 'Z')
  end <- generate 3 ('0', '9')
  return $ start ++ end
