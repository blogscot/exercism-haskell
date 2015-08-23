module BankAccount ( BankAccount
                   , openAccount
                   , closeAccount
                   , getBalance
                   , incrementBalance) where


import Control.Concurrent

type Account = Int
type Balance = Integer
data BankAccount = BankAccount Account Balance
  deriving (Eq, Show)

openAccount :: IO BankAccount
openAccount = return $ BankAccount 100 0

closeAccount :: BankAccount -> IO ()
closeAccount acc = return ()

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount _ bal) = return $ Just bal

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount acc bal) amount = return $ Just (bal + amount)


main = do
  acc <- openAccount
  getBalance acc >>= print
  bal <- incrementBalance acc 56
  print bal
  -- getBalance acc >>= print