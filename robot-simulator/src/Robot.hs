module Robot
    ( Bearing(East,
              North,
              South,
              West)
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

data Robot = Robot Bearing Position 
             deriving (Eq, Show)


bearing :: Robot -> Bearing
bearing (Robot dir pos) = dir

coordinates :: Robot -> Position
coordinates (Robot dir pos) = pos

mkRobot :: Bearing -> Position -> Robot
mkRobot direction coords = 
                Robot direction coords

move :: Robot -> String -> Robot
move = foldl movement 
    where 
        movement (Robot North (x, y)) 'A' = 
                Robot North (x, y+1)
        movement (Robot East  (x, y)) 'A' = 
                Robot East  (x+1, y)
        movement (Robot South (x, y)) 'A' = 
                Robot South (x, y-1)
        movement (Robot West  (x, y)) 'A' = 
                Robot West  (x-1, y)
        movement (Robot dir pos) 'R' = 
                Robot (turnRight dir) pos
        movement (Robot dir pos) 'L' = 
                Robot (turnLeft  dir) pos


turnLeft :: Bearing -> Bearing
turnLeft direction 
        | direction == North = West
        | otherwise = pred direction

turnRight :: Bearing -> Bearing
turnRight direction 
        | direction == West = North
        | otherwise = succ direction