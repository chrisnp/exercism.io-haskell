module CryptoSquare (encode) where

import Data.Char ( isAlphaNum, toLower )
import Data.List ( transpose )
import Control.Monad ( liftM2 )

encode :: String -> String
encode xs = 
    let 
        normalized :: [Char] -> [Char]
        normalized = filter isAlphaNum . map toLower
        squareSide :: [Char] -> Int
        squareSide ys = 
            let
                apprRoot = 
                    (floor . sqrt . (fromIntegral :: Int -> Double)) len
                len = 
                    length $ normalized ys
            in 
                if (apprRoot ^ 2) == len 
                then apprRoot 
                else apprRoot + 1
        segments :: [Char] -> [[Char]]
        segments = liftM2 chunksOf squareSide normalized
    in
        concat [ x ++ " " | x <- ((transpose . segments) xs) ]

-- Auxiliary 

chunksOf :: Int -> [e] -> [[e]]
-- chunksOf = (. (build . splitter)) . map . take
chunksOf i ls =
    let
        splitter :: [e] -> ([e] -> a -> a) -> a -> a
        splitter [] _ n = n
        splitter l c n  = l `c` splitter (drop i l) c n
        build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
        build = flip ($ (:)) ([])
    in
        map (take i) $ (build . splitter) ls
