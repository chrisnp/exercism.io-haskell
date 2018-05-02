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
        year = 31557600 
    in case planet of
        Mercury -> seconds / year / 0.2408467
        Venus -> seconds / year / 0.61519726
        Mars -> seconds / year / 1.8808158
        Jupiter -> seconds / year / 11.862615
        Saturn -> seconds / year / 29.447498
        Uranus -> seconds / year / 84.016846
        Neptune -> seconds / year / 164.79132
        Earth -> seconds / year