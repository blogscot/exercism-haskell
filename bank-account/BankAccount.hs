module BankAccount ( BankAccount
                   , openAccount
                   , closeAccount
                   , getBalance
                   , incrementBalance) where

import Control.Concurrent.STM

type Account = Int
type Balance = Integer
data BankAccount = BankAccount Account (TVar Balance)

openAccount :: IO BankAccount
openAccount = do
  bal <- atomically $ newTVar 0
  return $ BankAccount 100 bal

closeAccount :: BankAccount -> IO ()
closeAccount acc = return ()

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount acc bal) = atomically $ readTVar bal >>= return . Just

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount acc bal) amount = do
  current <- atomically $ readTVar bal
  let newBalance = current + amount
  atomically $ writeTVar bal newBalance
  return $ Just newBalance

main = do
  acc <- openAccount
  getBalance acc >>= print
  bal <- incrementBalance acc 56
  print bal
  getBalance acc >>= print