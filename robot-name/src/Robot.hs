module Robot (Robot, mkRobot, resetName, robotName) where

import Control.Concurrent.STM 
import Control.Monad 
import System.Random 
import System.IO.Unsafe
import Data.IORef
import qualified Data.Set as S

newtype Robot = Robot (TVar String)

namesList :: IORef (S.Set String)
namesList = unsafePerformIO $ newIORef S.empty

generateName :: IO [Char]
generateName =
    do letrs <- replicateM 2 $ randomRIO ('A', 'Z')
       nums  <- replicateM 3 $ randomRIO ('0', '9')
       let name = letrs ++ nums
       exists <- atomicModifyIORef' namesList $ \names -> 
                        if S.member name names then (names, True)
                        else (S.insert name names, False)
       if exists then generateName else return name

mkRobot :: IO Robot
mkRobot = liftM Robot (generateName >>= atomically . newTVar)
    
resetName :: Robot -> IO ()
resetName (Robot robot) = generateName >>= atomically . writeTVar robot 

robotName :: Robot -> IO String
robotName (Robot robot) = atomically (readTVar robot)
