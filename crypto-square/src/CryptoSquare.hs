module CryptoSquare (encode) where

import Data.Char (toLower, isAlphaNum)
import Data.List (transpose, unfoldr, intercalate )

encode :: String -> String
encode = 
    let 
        normalized :: String -> String
        normalized = filter isAlphaNum . map toLower
        rectangleSide :: (Integral b, Foldable t) => t a -> b
        rectangleSide = ceiling . (sqrt :: Double -> Double) 
                        . fromIntegral . length
        splitRows :: [Char] -> [[Char]]
        splitRows = takeWhile (not . null) 
                    . (unfoldr =<< (Just .) . splitAt . rectangleSide)
        pad :: [[Char]] -> [[Char]]
        pad = map =<< (. (++ " ")) . take . length . head
        joinChunks :: [[Char]] -> String
        joinChunks = intercalate " "
    in
        joinChunks . pad . transpose . splitRows . normalized

