module Queens (boardString, canAttack) where


board :: String
board = 
    let
        row = unwords $ replicate 8 "_"
    in
        unlines $ replicate 8 $ row

insert :: Char -> Maybe (Int, Int) -> String -> String
insert _ Nothing = id
insert q (Just (r, f)) = 
    let 
        pack :: [[Char]] -> String
        pack = 
            unlines . map (unwords . map(:[]))
        unpack :: String -> [[Char]]    
        unpack = 
            map (map head . words) . lines
        modify :: a -> Int -> Int -> [[a]] -> [[a]]
        modify item x y list = 
            first ++ 
            [row_first ++ [item] ++ row_last] 
            ++ rest
            where
                (first, (row:rest)) = 
                    splitAt x list
                (row_first, (_:row_last)) = 
                    splitAt y row
    in
        pack . modify q r f . unpack

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString = 
    flip (insert 'B') . ($ board) . insert 'W'

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack queenA queenB = 
    let
        (ax, ay) = 
            queenA
        (bx, by) = 
            queenB 
        dfile = 
            abs (ax - bx)
        drank = 
            abs (ay - by)
    in
        (==) 0 ((*) dfile drank) || 
        (==) 1 (div dfile drank)