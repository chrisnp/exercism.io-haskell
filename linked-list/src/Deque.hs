module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Control.Concurrent

data Deque a = Deque { _items :: MVar [a] }

mkDeque :: IO (Deque a)
mkDeque = do
    mvar <-
    

pop :: Deque a -> IO (Maybe a)
pop deque = error "You need to implement this function."

push :: Deque a -> a -> IO ()
push deque x = error "You need to implement this function."

unshift :: Deque a -> a -> IO ()
unshift deque x = error "You need to implement this function."

shift :: Deque a -> IO (Maybe a)
shift deque = error "You need to implement this function."
