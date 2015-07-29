module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

earthYear :: Double
earthYear = 365.25 * 24 * 60 * 60

ageOn :: Planet -> Double -> Double
ageOn Mercury x = x / earthYear / 0.2408467
ageOn Venus x   = x / earthYear / 0.61519726
ageOn Earth x   = x / earthYear
ageOn Mars x    = x / earthYear / 1.8808158
ageOn Jupiter x = x / earthYear / 11.862615
ageOn Saturn x  = x / earthYear / 29.447498
ageOn Uranus x  = x / earthYear / 84.016846
ageOn Neptune x = x / earthYear / 164.79132
