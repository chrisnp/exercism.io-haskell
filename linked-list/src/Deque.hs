module Deque ( Deque
             , mkDeque
             , pop
             , push
             , shift
             , unshift
             ) where

import Control.Concurrent

data Deque a = Deque { _items :: MVar [a] }

mkDeque :: IO (Deque a)
mkDeque = do
    mvar <- newMVar []
    return (Deque mvar)

pop :: Deque a -> IO (Maybe a)
pop deque = do
    let 
        mvar = _items deque
    items <- takeMVar mvar
    let x = case items of 
                [] -> Nothing
                items -> Just (last items)
    let xs = case items of
                [] -> []
                items -> init items
    putMVar mvar xs
    return x

push :: Deque a -> a -> IO ()
push deque x = do
    let mvar = _items deque
    items <- takeMVar mvar
    let _new = items ++ [x]
    putMVar mvar _new
    return ()

unshift :: Deque a -> a -> IO ()
unshift deque x = do
    let mvar = _items deque
    items <- takeMVar mvar
    let _new = [x] ++ items
    putMVar mvar _new
    return ()

shift :: Deque a -> IO (Maybe a)
shift deque = do
    let mvar = _items deque
    items <- takeMVar mvar
    let x = case items of 
                [] -> Nothing
                items -> Just (head items)
    let xs = case items of
                [] -> []
                items -> tail items
    putMVar mvar xs
    return x
