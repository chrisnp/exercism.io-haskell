module Meetup (Weekday(..), 
               Schedule(..), 
               meetupDay) where

import Data.Time.Calendar (Day, 
                           fromGregorian, 
                           gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Weekday = Monday 
               | Tuesday 
               | Wednesday 
               | Thursday 
               | Friday 
               | Saturday 
               | Sunday
               deriving (Eq, Show)

data Schedule = First 
                | Second 
                | Third 
                | Fourth 
                | Last 
                | Teenth

toWeekDay :: Day -> Weekday
toWeekDay d = 
  let 
    third (_,_,z) = z 
  in 
    case third (toWeekDate d) of
      1 -> Monday
      2 -> Tuesday 
      3 -> Wednesday 
      4 -> Thursday 
      5 -> Friday 
      6 -> Saturday
      7 -> Sunday

              
meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = 
  let 
    monthLength = gregorianMonthLength year month
    inRange = head . filter ((== weekday) . toWeekDay) . dates
      where 
        dates = map $ fromGregorian year month
  in case schedule of
    Teenth -> inRange [13..19]
    First  -> inRange [1..7]
    Second -> inRange [8..14]
    Third  -> inRange [15..21]
    Fourth -> inRange [22..29]
    Last   -> 
      inRange [monthLength, monthLength - 1..monthLength - 7]