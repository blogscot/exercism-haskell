module Robot (robotName, mkRobot, resetName) where

import System.Random (randomRIO)
import Control.Concurrent (MVar, newMVar, readMVar, swapMVar)
import Control.Monad (liftM, void)

newtype Robot = Robot {getName :: MVar String}

mkRobot :: IO Robot
mkRobot = liftM Robot $ generateName >>= newMVar

resetName :: Robot -> IO ()
resetName s = generateName >>= void . swapMVar (getName s)

robotName :: Robot -> IO String
robotName = readMVar . getName

generateName :: IO String
generateName = mapM randomRIO pattern
  where pattern = [ letter, letter, digit, digit, digit ]
        letter = ('A', 'Z')
        digit = ('0', '9')
