module Robot (robotName, mkRobot, resetName) where

import System.Random (newStdGen, randomRs)
import Control.Concurrent (MVar, newMVar, readMVar, swapMVar)
import Control.Monad (void)

mkRobot :: IO (MVar String)
mkRobot = generateName >>= newMVar

resetName :: MVar String -> IO ()
resetName s =
  void . swapMVar s =<< generateName

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
