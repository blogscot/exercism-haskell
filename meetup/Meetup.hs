module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar

type Year = Int
type Month = Int

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Enum, Show)

data Schedule = First | Second | Third | Fourth | Teenth | Last
  deriving (Enum, Show)

meetupDay :: Schedule -> Weekday -> Year -> Month -> Day
meetupDay First wd y m = head $ daysOfWeek wd y m
meetupDay Second wd y m = daysOfWeek wd y m !! 1
meetupDay Third  wd y m = daysOfWeek wd y m !! 2
meetupDay Fourth wd y m = daysOfWeek wd y m !! 3
meetupDay Last wd y m = last $ daysOfWeek wd y m
meetupDay Teenth wd y m = head [ date | date <- daysOfWeek wd y m, check $ toGregorian date]
  where check (_, _, day) = day >= 13 && day <= 19

offset :: Integer -> Int -> Int -> Weekday
offset year month day = toEnum offset'
  where ref = fromGregorian 2015 08 10 -- a Monday, i.e. the start of the week
        old = fromGregorian year month day
        offset' = fromInteger (diffDays old ref) `mod` 7

weekdays :: Integer -> Int -> [(Weekday, Int)]
weekdays year month = [(offset year month day, day) | day <- [1..monthLength]]
  where monthLength = gregorianMonthLength year month

daysOfWeek :: Weekday -> Year -> Month -> [Day]
daysOfWeek wd y m = [fromGregorian (toInteger y) m day | (weekday, day)
                      <- weekdays (toInteger y) m, weekday == wd]
