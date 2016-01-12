module Data.Duration
    ( humanReadableDuration
    -- durations
    , ms
    , oneSecond
    , minute
    , hour
    , day
    , year
    -- Retrieve
    , getMs
    , getSeconds
    , getMinutes
    , getHours
    , getDays
    , getYears
    ) where

import Data.Fixed (Fixed(..), Micro, div', mod')

-- | `humanReadableDuration` take some time in micro-second precision and render a human readable duration.
--
--    > let duration = 2 * ms + 3 * oneSecond + 2 * minute + 33*day + 2*year
--    > humanReadableDuration duration
--    > -- will return: "2 years 33 days 2 min 3s 32ms"
humanReadableDuration :: Micro -> String
humanReadableDuration n
  | n < oneSecond = let mi = getMs      n in if mi > 0 then show mi ++ "ms" else ""
  | n < minute = let s  = getSeconds n in if s  > 0 then show s  ++ "s " ++ humanReadableDuration (n `mod'` oneSecond) else ""
  | n < hour   = let m  = getMinutes n in if m  > 0 then show m  ++ " min " ++ humanReadableDuration (n `mod'` minute) else ""
  | n < day    = let h  = getHours   n in if h  > 0 then show h  ++ " hours " ++ humanReadableDuration (n `mod'` hour) else ""
  | n < year   = let d  = getDays    n in if d  > 0 then show d  ++ " days " ++ humanReadableDuration (n `mod'` day) else ""
  | otherwise  = let y  = getYears   n in if y  > 0 then show y  ++ " years " ++ humanReadableDuration (n `mod'` year) else ""

--------------------------------------------------------------------------------
-- Durations
--------------------------------------------------------------------------------

-- | number of micro seconds in one millisecond
ms :: Micro
ms = MkFixed 1000

-- | number of micro seconds in one second
oneSecond :: Micro
oneSecond = 1000 * ms

-- | number of micro seconds in one minute
minute :: Micro
minute = 60 * oneSecond

-- | number of micro seconds in one hour
hour :: Micro
hour = 60 * minute

-- | number of micro seconds in one day
day :: Micro
day = 24 * hour

-- | number of micro seconds in one year
year :: Micro
year = 365 * day

--------------------------------------------------------------------------------
-- Retrieve some durations
--------------------------------------------------------------------------------
-- | number of milli seconds given a duration in micro seconds
getMs :: Micro -> Integer
getMs n = n `div'` ms

-- | number of seconds given a duration in micro seconds
getSeconds :: Micro -> Integer
getSeconds n = n `div'` oneSecond

-- | number of minutes given a duration in micro seconds
getMinutes :: Micro -> Integer
getMinutes n = n `div'` minute

-- | number of hours given a duration in micro seconds
getHours :: Micro -> Integer
getHours n = n `div'` hour

-- | number of days given a duration in micro seconds
getDays :: Micro -> Integer
getDays n = n `div'` day

-- | number of years given a duration in micro seconds
getYears :: Micro -> Integer
getYears n = n `div'` year
