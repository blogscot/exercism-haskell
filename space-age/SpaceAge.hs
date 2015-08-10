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

ageOn' :: Double -> Double -> Double
ageOn' secs ratio = secs / ratio / earthYear

ageOn :: Planet -> Double -> Double
ageOn Earth secs   = secs / earthYear
ageOn Mercury secs = ageOn' secs 0.2408467
ageOn Venus secs   = ageOn' secs 0.61519726
ageOn Mars secs    = ageOn' secs 1.8808158
ageOn Jupiter secs = ageOn' secs 11.862615
ageOn Saturn secs  = ageOn' secs 29.447498
ageOn Uranus secs  = ageOn' secs 84.016846
ageOn Neptune secs = ageOn' secs 164.79132
