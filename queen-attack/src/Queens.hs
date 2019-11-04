module Queens (boardString, canAttack) where

board :: String
board = unlines $ replicate 8 $ row
    where 
        row = unwords $ replicate 8 "_"

insert :: Char -> Maybe (Int, Int) -> String -> String
insert _ Nothing = id
insert c (Just (x, y)) = pack . modify c x y . unpack
    where 
        pack :: [[Char]] -> String
        pack = unlines . map (unwords . map(:[]))
        unpack :: String -> [[Char]]    
        unpack = map (map head . words) . lines
        modify :: a -> Int -> Int -> [[a]] -> [[a]]
        modify item x y list = first ++ [row_first ++ [item] ++ row_last] ++ rest
            where
                (first, (row:rest)) = splitAt x list
                (row_first, (_:row_last)) = splitAt y row


boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = insert 'B' black $ insert 'W' white $ board

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack queenA queenB = dfile * drank == 0 || 
                          dfile `div` drank == 1
    where 
        (ax, ay) = queenA
        (bx, by) = queenB 
        dfile = abs (ax - bx)
        drank = abs (ay - by)