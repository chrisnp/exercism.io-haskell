module Robot (Robot, mkRobot, resetName, robotName) where

import Data.IORef
import System.Random

type Robot = IORef String

mkRobot :: IO Robot
mkRobot = do
    name <- generateName
    newIORef name

generateName :: IO String
generateName = do
    gen <- newStdGen
    let name = take 2 (randomRs ('A','Z') gen) ++ take 3 (randomRs ('0','9') gen)
    return name

resetName :: Robot -> IO ()
resetName robot = do
    name <- generateName
    writeIORef robot name

robotName :: Robot -> IO String
robotName robot = readIORef robot
