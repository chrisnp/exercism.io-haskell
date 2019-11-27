module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = 
    let
        earthYear = 31557600 
    in case planet of
        Mercury -> seconds / earthYear / 0.2408467
        Venus   -> seconds / earthYear / 0.61519726
        Earth   -> seconds / earthYear
        Mars    -> seconds / earthYear / 1.8808158
        Jupiter -> seconds / earthYear / 11.862615
        Saturn  -> seconds / earthYear / 29.447498
        Uranus  -> seconds / earthYear / 84.016846
        Neptune -> seconds / earthYear / 164.79132   