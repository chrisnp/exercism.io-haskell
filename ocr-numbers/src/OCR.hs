module OCR (convert) where

import Data.List (elemIndex, transpose, intercalate)
import Data.List.Split (chunksOf)

convert :: String -> String
convert = intercalate "," 
          . map convertLine 
          . chunksOf 4 
          . lines

-- Auxiliary

convertLine :: [String] -> String
convertLine = concat 
              . map convertDigit 
              . transpose 
              . map (chunksOf 3)  

convertDigit :: [String] -> String
convertDigit digit = 
    case elemIndex digit ocrDigits of
      Just n  -> show n
      Nothing -> "?"

ocrDigits :: [[String]]
ocrDigits = [
    [ " _ "
    , "| |"
    , "|_|"
    , "   " ], [ "   "
               , "  |"
               , "  |"
               , "   "], 
    [ " _ "
    , " _|"
    , "|_ "
    , "   "], [ " _ "
              , " _|"
              , " _|"
              , "   " ],
    [ "   "
    , "|_|"
    , "  |"
    , "   " ], [ " _ "
               , "|_ "
               , " _|"
               , "   " ],
    [ " _ "
    , "|_ "
    , "|_|"
    , "   "], [ " _ "
              , "  |"
              , "  |"
              , "   " ],
    [ " _ "
    , "|_|"
    , "|_|"
    , "   " ], [ " _ "
               , "|_|"
               , " _|"
               , "   "]
  ]