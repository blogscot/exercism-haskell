module Meetup (Weekday(..), Schedule) where

import Data.Time.Calendar

type Year = Int
type Month = Int

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Enum, Show)

data Schedule = First | Second | Third | Fourth | Teenth | Last
  deriving (Enum, Show)

meetupDay :: Schedule -> Weekday -> Year -> Month -> Day
meetupDay = undefined
-- meetupDay sch wk y m
--   | First _ _ _ = weekdays y m

offset :: Integer -> Int -> Int -> Weekday
offset year month day = toEnum offset'
  where ref = fromGregorian 2015 08 10 -- a Monday
        old = fromGregorian year month day
        offset' = fromInteger (diffDays old ref) `mod` 7

weekdays :: Integer -> Int -> [(Weekday, Int)]
weekdays year month = [(offset year month day, day) | day <- [1..monthLength]]
  where monthLength = gregorianMonthLength year month

daysofweek :: Weekday -> Year -> Month -> [(Weekday, Int)]
daysofweek wkdy y m = [ (weekday, day) | (weekday, day) <- weekdays (toInteger y) m, weekday == wkdy]


main = do
  -- print $ fromEnum Teenth
  -- print $ ModifiedJulianDay 57245
  -- print $ toGregorian $ fromGregorian 2011 12 24
  -- print $ weekdays 2015 08
  -- print [ (weekday, day) | (weekday, day) <- weekdays 2015 08, weekday == Saturday]
  print $ daysofweek Saturday 2015 08