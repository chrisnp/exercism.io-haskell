module Robot
    ( Bearing (East, North, South, West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum)

type Position = (Integer, Integer)

data Robot = Robot Bearing Position deriving (Eq, Show)


bearing :: Robot -> Bearing
bearing (Robot dir _) = dir

coordinates :: Robot -> Position
coordinates (Robot _ pos) = pos

mkRobot :: Bearing -> Position -> Robot
mkRobot direction position = Robot direction position

move :: Robot -> String -> Robot
move =  
    let 
        movement (Robot North (x, y)) 'A' = Robot North (x, y + 1)
        movement (Robot East  (x, y)) 'A' = Robot East  (x + 1, y)
        movement (Robot South (x, y)) 'A' = Robot South (x, y - 1)
        movement (Robot West  (x, y)) 'A' = Robot West  (x - 1, y)
        movement (Robot dir pos) 'R' = Robot (turnRight dir) pos
        movement (Robot dir pos) 'L' = Robot (turnLeft  dir) pos
        movement (Robot dir pos)  _  = Robot dir pos
    in
        foldl movement

-- Auxiliary 

turnLeft :: Bearing -> Bearing
turnLeft direction = 
    if direction == North then West else pred direction

turnRight :: Bearing -> Bearing
turnRight direction = 
    if direction == West then North else succ direction