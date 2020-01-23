module Beer (song) where


bottles :: Int -> String
bottles n 
    | n == 0    = "no more bottles"
    | n == 1    = "1 bottle"
    | otherwise = show n ++ " bottles"



verse :: Int -> String
verse n
    | n == 0    = "No more bottles of beer on the wall, no more bottles of beer.\n\
                  \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
    | otherwise = bottles n ++ 
                  " of beer on the wall, " ++
                  bottles n ++ 
                  " of beer.\n\
                  \Take " ++ 
                  down n ++ 
                  " down and pass it around, " ++ 
                  bottles (n-1) ++ 
                  " of beer on the wall.\n\
                  \\n"
    where 
        down x 
            | x == 1    = "it"
            | otherwise = "one"


song :: String
song = foldl (\acc x -> verse x ++ acc) "" [0..99]