module Series (Error(..), largestProduct) where

import Control.Monad ((>=>), liftM2)
import Data.Char     (digitToInt, isDigit)
import Data.List     (tails)
import Data.Maybe    (mapMaybe)
import Safe          (maximumMay)
import Safe.Exact    (takeExactMay)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size = 
    traverse charToNum >=> rightElse InvalidSpan 
    . maximumMay 
    . (mapMaybe (fmap product . takeExactMay (fromIntegral size )) 
       . tails)

-- Auxiliary

rightElse :: a -> Maybe b -> Either a b
rightElse a Nothing  = Left a
rightElse _ (Just x) = Right x

if' :: Bool -> p -> p -> p
if' True  x _ = x
if' False _ y = y

charToNum :: Num b => Char -> Either Error b
charToNum = (liftM2 if' isDigit (Right . fromIntegral . digitToInt)) 
            <*> 
            (Left . InvalidDigit)
