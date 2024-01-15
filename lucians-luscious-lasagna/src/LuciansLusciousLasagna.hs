module LuciansLusciousLasagna 
    ( elapsedTimeInMinutes
    , expectedMinutesInOven
    , preparationTimeInMinutes
    ) where

expectedMinutesInOven :: Num a => a
expectedMinutesInOven = 40

preparationTimeInMinutes :: Num a => a -> a
preparationTimeInMinutes = (*) 2 

elapsedTimeInMinutes :: Num a => a -> a -> a
elapsedTimeInMinutes = (+) . (*) 2 