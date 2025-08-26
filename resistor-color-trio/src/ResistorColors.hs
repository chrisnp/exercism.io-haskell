module ResistorColors (
                      Color(..), 
                      Resistor(..), 
                      label, 
                      ohms
                      ) where

-- API 

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

colorValue :: Color -> Int
colorValue = fromEnum

ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) = 
  let 
    baseValue = colorValue c1 * 10 + colorValue c2
    multiplier = 10 ^ colorValue c3
  in 
    baseValue * multiplier

-- Auxiliary fcns

label :: Resistor -> String
label = formatOhms . ohms

formatOhms :: Int -> String
formatOhms = 
    let  
      divisibleBy :: Int -> Int -> Bool
      divisibleBy = 
        flip flip 0 . ((==) .) . flip mod
      formatWithUnit :: Int -> String -> Int -> String
      formatWithUnit = 
        (. (' ' :)) . flip . (((++) . show) .) . flip div
    in 
      \value -> case () of
        _ | value >= 1000000000 && divisibleBy 1000000000 value -> 
              formatWithUnit 1000000000 "gigaohms" value
          | value >= 1000000 && divisibleBy 1000000 value -> 
              formatWithUnit 1000000 "megaohms" value
          | value >= 1000 && divisibleBy 1000 value -> 
              formatWithUnit 1000 "kiloohms" value
          | otherwise -> 
              formatWithUnit 1 "ohms" value
