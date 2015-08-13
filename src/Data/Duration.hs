module Data.Duration
    ( humanReadableDuration
    -- durations
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


-- | `humanReadableDuration` take some time in micro-seconds and render a human readable duration.
-- Typically:
-- humanReadableDuration (10^6 * 60) --> 1 min
-- This isn't made to be fast. It uses String.
humanReadableDuration :: Int -> String
humanReadableDuration n
  | n < oneSecond = let ms = getMs      n in if ms > 0 then show ms ++ "ms" else ""
  | n < minute = let s  = getSeconds n in if s  > 0 then show s  ++ "s " ++ humanReadableDuration (n `rem` oneSecond) else ""
  | n < hour   = let m  = getMinutes n in if m  > 0 then show m  ++ " min " ++ humanReadableDuration (n `rem` minute) else ""
  | n < day    = let h  = getHours   n in if h  > 0 then show h  ++ " hours " ++ humanReadableDuration (n `rem` hour) else ""
  | n < year   = let d  = getDays    n in if d  > 0 then show d  ++ " days " ++ humanReadableDuration (n `rem` day) else ""
  | otherwise  = let y  = getYears   n in if y  > 0 then show y  ++ " years " ++ humanReadableDuration (n `rem` year) else ""

--------------------------------------------------------------------------------
-- Durations
--------------------------------------------------------------------------------
-- | number of micro seconds in one second
oneSecond :: Int
oneSecond = 10^(6 :: Integer)

-- | number of micro seconds in one minute
minute :: Int
minute = 60 * oneSecond

-- | number of micro seconds in one hour
hour :: Int
hour = 60 * minute

-- | number of micro seconds in one day
day :: Int
day = 24 * hour

-- | number of micro seconds in one year
year :: Int
year = 365 * day

--------------------------------------------------------------------------------
-- Retrieve some durations
--------------------------------------------------------------------------------
-- | number of milli seconds given a duration in micro seconds
getMs :: Int -> Int
getMs n = (n `rem` oneSecond) `div` 1000

-- | number of seconds given a duration in micro seconds
getSeconds :: Int -> Int
getSeconds n = (n `rem` minute) `div` oneSecond

-- | number of minutes given a duration in micro seconds
getMinutes :: Int -> Int
getMinutes n = (n `rem` hour) `div` minute

-- | number of hours given a duration in micro seconds
getHours :: Int -> Int
getHours n = n `div` hour

-- | number of days given a duration in micro seconds
getDays :: Int -> Int
getDays n = n `div` day

-- | number of years given a duration in micro seconds
getYears :: Int -> Int
getYears n = n `div` year