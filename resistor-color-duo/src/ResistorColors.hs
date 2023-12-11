module ResistorColors (Color(..), value) where

data Color = Black
             | Brown
             | Red
             | Orange
             | Yellow
             | Green
             | Blue
             | Violet
             | Grey
             | White
             deriving (Eq, Show, Enum, Bounded)

value :: (Color, Color) -> Int
value = uncurry ((. fromEnum) . (+) . (10 *) . fromEnum)
