{-# LANGUAGE NamedFieldPuns #-}

module Canteven.ParseDate.Types (
  LocalTimeAndOffset(..),
  ltaoInTimeZone,
  ltaoInTimeZoneTZ
) where

import Data.Time (UTCTime, ParseTime (buildTime))
import Data.Time.LocalTime (LocalTime, TimeZone, localTimeToUTC)
import Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries, utcToLocalTime')
import Data.Time.Zones (TZ, utcToLocalTimeTZ)


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
ltaoInTimeZone = ltaoInTimeZoneConvert utcToLocalTime'

-- | same as @ltaoInTimeZone@ but for TZ
ltaoInTimeZoneTZ :: TZ -> LocalTimeAndOffset -> LocalTime
ltaoInTimeZoneTZ = ltaoInTimeZoneConvert utcToLocalTimeTZ



ltaoInTimeZoneConvert :: (a -> UTCTime -> LocalTime) -> a -> LocalTimeAndOffset -> LocalTime
-- If the LTAO doesn't specify an offset, it must be local time, so just use it
-- directly.
ltaoInTimeZoneConvert _ _ (LocalTimeAndOffset lTime Nothing) = lTime
-- Otherwise, this LTAO specifies a UTCTime, so convert that UTCTime to LocalTime.
ltaoInTimeZoneConvert convert tz (LocalTimeAndOffset lTime (Just off)) =
    convert tz utc
  where
    utc = localTimeToUTC off lTime
