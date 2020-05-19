module Deque (Deque, 
              mkDeque, 
              pop, push, 
              shift, unshift) where

import Control.Concurrent

data Deque a = Deque { _items :: MVar [a] }

mkDeque :: IO (Deque a)
mkDeque = do
    mvar <- newMVar []
    let deque = Deque mvar
    return deque

pop :: Deque a -> IO (Maybe a)
pop deque = do
    let mvar = _items deque
    items <- takeMVar mvar
    let (x, xs) = 
        case items of
            [] -> 
                (Nothing, [])
            i  -> 
                (Just $ last item, init item)

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
    let _new = x : items
    putMVar mvar _new
    return ()

shift :: Deque a -> IO (Maybe a)
shift deque = do
    let mvar = _items deque
    items <- takeMVar mvar
    let (x, xs) = case items of 
            [] -> 
                (Nothing, [])
            i  -> 
                (Just $ head item, tail item)
    putMVar mvar xs
    return x
