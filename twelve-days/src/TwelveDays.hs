module TwelveDays (recite) where

import Control.Monad (liftM2, join)


recite :: Int -> Int -> [String]
recite start stop = 
    map (liftM2 (++) premise conclusion) 
        [start .. stop]

gifts :: [String]
gifts = [ "twelve Drummers Drumming, ",
          "eleven Pipers Piping, ",
          "ten Lords-a-Leaping, ",
          "nine Ladies Dancing, ",
          "eight Maids-a-Milking, ",
          "seven Swans-a-Swimming, ",
          "six Geese-a-Laying, ",
          "five Gold Rings, ",
          "four Calling Birds, ",
          "three French Hens, ",
          "two Turtle Doves, and ",
          "a Partridge in a Pear Tree." ]

nthDay :: [String]
nthDay = [ "xmasEve",
           "first", "second", 
           "third", "fourth", 
           "fifth", "sixth", 
           "seventh", "eighth", 
           "ninth", "tenth", 
           "eleventh", "twelfth" ]

premise :: Int -> String
premise = 
  let
    prologue = "On the "
    epilogue = 
      " day of Christmas my true love gave to me: "
  in
    (prologue ++) . (++ epilogue) . (nthDay !!)

conclusion :: Int -> String
conclusion = 
    flip (join .) gifts . drop . (-) (length gifts)