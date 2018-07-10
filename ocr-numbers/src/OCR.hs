module OCR (convert) where

import Data.List (elemIndex, transpose, intercalate)
import Data.List.Split (chunksOf)

convertDigit :: [String] -> String
convertDigit digit = 
    case elemIndex digit ocrDigits of
      Just n  -> show n
      Nothing -> "?"

convertLine :: [String] -> String
convertLine = concat . map convertDigit . transpose . map (chunksOf 3)     

convert :: String -> String
convert xs = (intercalate "," . map convertLine . chunksOf 4 . lines) xs

-- this has to be ordered from representation of 0 to representation of 9
ocrDigits :: [[String]]
ocrDigits = [
    [ " _ "
    , "| |"
    , "|_|"
    , "   " ],
    [ "   "
    , "  |"
    , "  |"
    , "   "],
    [ " _ "
    , " _|"
    , "|_ "
    , "   "],
    [ " _ "
    , " _|"
    , " _|"
    , "   " ],
    [ "   "
    , "|_|"
    , "  |"
    , "   " ],
    [ " _ "
    , "|_ "
    , " _|"
    , "   " ],
    [ " _ "
    , "|_ "
    , "|_|"
    , "   "],
    [ " _ "
    , "  |"
    , "  |"
    , "   " ],
    [ " _ "
    , "|_|"
    , "|_|"
    , "   " ],
    [ " _ "
    , "|_|"
    , " _|"
    , "   "]
  ]