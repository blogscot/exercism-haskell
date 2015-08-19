module Robot (robotName, mkRobot, resetName) where

import System.Random (newStdGen, randomRs)
import Control.Concurrent (MVar, newMVar, readMVar, swapMVar)
import Control.Monad (void)

data Robot = Robot {getName :: MVar String}

mkRobot :: IO Robot
mkRobot = generateName >>= newMVar >>= return . Robot

resetName :: Robot -> IO ()
resetName s =
  void . swapMVar (getName s) =<< generateName

robotName :: Robot -> IO String
robotName = readMVar . getName

generate :: Int -> (Char, Char) -> IO String
generate n (low, high) = do
  g <- newStdGen
  return $ take n $ randomRs(low, high) g

generateName :: IO String
generateName = do
  start <- generate 2 ('A', 'Z')
  end <- generate 3 ('0', '9')
  return $ start ++ end
