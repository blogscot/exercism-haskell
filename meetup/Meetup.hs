module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar

type Year = Int
type Month = Int

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Enum, Show)

data Schedule = First | Second | Third | Fourth | Teenth | Last
  deriving (Enum, Show)

meetupDay :: Schedule -> Weekday -> Year -> Month -> Day
meetupDay First wd y m = snd . head $ daysOfWeek wd y m
meetupDay Second wd y m = snd . head . tail $ daysOfWeek wd y m
meetupDay Third  wd y m = snd . head . tail . tail $ daysOfWeek wd y m
meetupDay Fourth wd y m = snd . head . tail . tail . tail $ daysOfWeek wd y m
meetupDay Last wd y m = snd . last $ daysOfWeek wd y m
meetupDay Teenth wd y m = head [ date |(_, date) <- daysOfWeek wd y m, check $ show date]
  where day date = read $ drop 8 date
        check date = day date >= 13 && day date <= 19

offset :: Integer -> Int -> Int -> Weekday
offset year month day = toEnum offset'
  where ref = fromGregorian 2015 08 10 -- a Monday
        old = fromGregorian year month day
        offset' = fromInteger (diffDays old ref) `mod` 7

weekdays :: Integer -> Int -> [(Weekday, Int)]
weekdays year month = [(offset year month day, day) | day <- [1..monthLength]]
  where monthLength = gregorianMonthLength year month

daysOfWeek :: Weekday -> Year -> Month -> [(Weekday, Day)]
daysOfWeek wd y m = [ (weekday, fromGregorian (toInteger y) m day) | (weekday, day)
                      <- weekdays (toInteger y) m, weekday == wd]


-- main = do
  -- print $ fromEnum Teenth
  -- print $ ModifiedJulianDay 57245
  -- print $ toGregorian $ fromGregorian 2011 12 24
  -- print $ weekdays 2015 08
  -- print [ (weekday, day) | (weekday, day) <- weekdays 2015 08, weekday == Saturday]
  -- print $ daysOfWeek Saturday 2015 08
  -- print $ snd . head $ daysOfWeek Saturday 2015 08
  -- print $ snd . head . tail $ daysOfWeek Saturday 2015 08
  -- print $ snd . last $ daysOfWeek Saturday 2015 08
  -- print [ date |(_, date) <- daysOfWeek Saturday 1967 08, check $ show date]
  --   where day date = read $ drop 8 date
  --         check date  = day date >= 13 && day date <= 19
  -- print $ meetupDay First Monday 2015 08
  -- print $ meetupDay Last Monday 2015 08
  -- print $ meetupDay Teenth Monday 2015 08