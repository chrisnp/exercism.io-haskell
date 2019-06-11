module ResistorColors (Color(..), value) where

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
  deriving (Eq, Show, Read)

colorValue :: Color -> Int
colorValue color = 
  case color of
    Black  -> 0
    Brown  -> 1
    Red    -> 2
    Orange -> 3
    Yellow -> 4
    Green  -> 5
    Blue   -> 6
    Violet -> 7
    Grey   -> 8
    White  -> 9

value :: [Color] -> Int
value cs = foldl ((+) . (*10)) 0 (colorValue <$> cs)