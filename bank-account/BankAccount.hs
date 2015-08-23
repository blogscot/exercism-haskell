module BankAccount ( BankAccount
                   , openAccount
                   , closeAccount
                   , getBalance
                   , incrementBalance) where

import Control.Concurrent.STM
import Control.Monad

type Balance = Integer
data BankAccount = BankAccount (TVar (Maybe Balance))

openAccount :: IO BankAccount
openAccount = do
  bal <- atomically . newTVar $ Just 0
  return $ BankAccount bal

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount bal) = atomically $ writeTVar bal Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount bal) = atomically $ readTVar bal

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount bal) amount = do
  current <- atomically $ readTVar bal
  let newBalance = liftM (+amount) current
  atomically $ writeTVar bal newBalance
  return newBalance
