module Robot (Robot, mkRobot, resetName, robotName) where

import Control.Concurrent.STM 
import Control.Monad 
import System.Random 

newtype Robot = Robot (TVar String)

mkRobot :: IO Robot
mkRobot = liftM Robot (generateName >>= atomically . newTVar)

generateName :: IO String
generateName = mapM randomRIO [letr, letr, num, num, num] 
    where
        letr = ('A', 'Z')
        num  = ('0', '9')

resetName :: Robot -> IO ()
resetName (Robot robot) = generateName >>= atomically . writeTVar robot 

robotName :: Robot -> IO String
robotName (Robot robot) = atomically (readTVar robot)
