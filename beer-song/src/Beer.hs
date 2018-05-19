module Beer (song) where


beers :: Int -> String
beers n 
    | n == 0    = "no more bottles"
    | n == 1    = "1 bottle"
    | otherwise = show n ++ " bottles"


verse :: Int -> String
verse n
    | n == 0    = "No more bottles of beer on the wall, no more bottles of beer.\n\
                \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
    | otherwise = beers n ++ " of beer on the wall, " ++ beers n ++ " of beer.\n\
                \Take " ++ down n ++ " down and pass it around, " ++ beers (n-1) ++ " of beer on the wall.\n\
                \\n"
    where 
        down n 
            | n == 1    = "it"
            | otherwise = "one"

song :: String
song = foldl (\acc x -> verse x ++ acc) "" [0..99]
