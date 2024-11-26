module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.MVar
import Control.DeepSeq (($!!))

data BankAccount = BankAccount { _balance :: MVar (Maybe Integer) }

closeAccount :: BankAccount -> IO ()
closeAccount account = do
    let balance = _balance account
    _ <- swapMVar balance Nothing
    return ()

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance account = do
    let balance = _balance account
    readMVar balance

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = do
    let balance = _balance account
    oldBalance <- takeMVar balance
    let newBalance = (+ amount) <$> oldBalance
    putMVar  balance $!! newBalance
    return newBalance

openAccount :: IO BankAccount
openAccount = do
    deposit <- newMVar (Just 0)
    let 
        balance = BankAccount deposit
    return balance
