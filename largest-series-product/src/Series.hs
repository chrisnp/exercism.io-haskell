module Series (Error(..), largestProduct) where

import Control.Monad    ((>=>))
import Data.Char        (digitToInt, isDigit)
import Data.List        (tails)
import Data.Maybe       (mapMaybe)
import Safe             (maximumMay)
import Safe.Exact       (takeExactMay)

data Error = InvalidSpan 
             | InvalidDigit Char 
             deriving (Show, Eq)

rightElse :: a -> Maybe b -> Either a b
rightElse a Nothing  = 
    Left a
rightElse _ (Just x) = 
    Right x

charToNum :: Num b => Char -> Either Error b
charToNum ch
    | isDigit ch = 
        Right . fromIntegral . digitToInt $ ch
    | otherwise  = 
        Left (InvalidDigit ch)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size = 
    traverse charToNum >=> rightElse InvalidSpan 
    . maximumMay 
    . (mapMaybe (fmap product 
                 . takeExactMay (fromIntegral size)) 
                 . tails)

       