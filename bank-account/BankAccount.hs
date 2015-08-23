module BankAccount ( BankAccount
                   , openAccount
                   , closeAccount
                   , getBalance
                   , incrementBalance) where

import Control.Concurrent.STM

type Account = Int
type Balance = Integer
data BankAccount = BankAccount Account (TVar (Maybe Balance))

openAccount :: IO BankAccount
openAccount = do
  bal <- atomically $ newTVar (Just 0)
  return $ BankAccount 100 bal

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount _ bal) = atomically $ writeTVar bal Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount _ bal) = atomically $ readTVar bal

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount _ bal) amount = do
  current <- atomically $ readTVar bal
  let newBalance = fmap (+amount) current
  atomically $ writeTVar bal newBalance
  return newBalance

-- main = do
--   acc <- openAccount
--   getBalance acc >>= print
--   bal <- incrementBalance acc 56
--   print bal
--   getBalance acc >>= print