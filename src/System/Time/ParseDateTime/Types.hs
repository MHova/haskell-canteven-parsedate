{-# LANGUAGE NamedFieldPuns #-}

module System.Time.ParseDateTime.Types (
    LocalTimeAndOffset(..), ltaoInTimeZone
    ) where

import           Data.Time                           (ParseTime (buildTime))
import           Data.Time.LocalTime                 (LocalTime, TimeZone,
                                                      localTimeToUTC)
import           Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries,
                                                      utcToLocalTime')

-- | A data type for parsing ISO8601 and distinguishing between with-offset or
-- without-offset formats.
data LocalTimeAndOffset = LocalTimeAndOffset {
      ltaoLocalTime :: LocalTime
    , ltaoOffset    :: Maybe TimeZone
    } deriving (Read, Show, Eq)

instance ParseTime LocalTimeAndOffset where
    buildTime l xs =
        LocalTimeAndOffset (buildTime l $ filter (not . isZoneInfo) xs)
            maybeZone
      where
        -- Apparently "%Z" matches even the empty string and parses it as 00:00.
        -- "%z" might do this too but I haven't tested it.
        isZoneInfo ('z',_) = True
        isZoneInfo ('Z',"") = False
        isZoneInfo ('Z',_) = True
        isZoneInfo _ = False
        zoneLetters = filter isZoneInfo xs
        maybeZone | null zoneLetters = Nothing
                  | otherwise = Just $ buildTime l zoneLetters

-- | Get the localtime represented by this LTAO in the given timezone.
ltaoInTimeZone :: TimeZoneSeries -> LocalTimeAndOffset -> LocalTime
-- If the LTAO doesn't specify an offset, it must be local time, so just use it
-- directly.
ltaoInTimeZone _ LocalTimeAndOffset { ltaoLocalTime,
                                      ltaoOffset = Nothing } = ltaoLocalTime
-- Otherwise, this LTAO specifies a UTCTime, so convert that UTCTime to LocalTime.
ltaoInTimeZone tz LocalTimeAndOffset { ltaoLocalTime, ltaoOffset = Just offset } =
    utcToLocalTime' tz utc
  where
    utc = localTimeToUTC offset ltaoLocalTime
