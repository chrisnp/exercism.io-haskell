module CryptoSquare (encode) where

import Data.Char ( isAlphaNum, toLower )
import Data.List ( transpose, intersperse, intercalate )
import Data.List.Split ( chunksOf )


encode :: String -> String
encode xs = 
    let 
        normalized :: [Char] -> [Char]
        normalized ys = ( filter isAlphaNum . map toLower ) ys
        squareSide :: [Char] -> Int
        squareSide ys = if (apprRoot ^ 2) == len then apprRoot else apprRoot + 1
                where 
                    apprRoot = (floor . sqrt . (fromIntegral :: Int -> Double)) len
                    len = length $ normalized ys
        segments :: [Char] -> [[Char]]
        segments ys = chunksOf (squareSide ys) (normalized ys)
    in
        concat [ x ++ " " | x <- ((transpose . segments) xs) ]
