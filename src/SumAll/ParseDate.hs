{- |
This module wraps `Date.Time.Format` and provides a handy way to parse a date
string who's format is unknown, but which may be one of several "supported"
date formats.
-}
module SumAll.ParseDate (
    parseDate,
    supportedDateFormats,
    iso8601UtcDateFormats,
    twitterDateFormat
) where

import Data.Maybe(mapMaybe, listToMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime)
import System.Locale (rfc822DateFormat, defaultTimeLocale)

-- Public Types ---------------------------------------------------------------
-- Semi-Public Types ----------------------------------------------------------
-- Public Functions -----------------------------------------------------------

{- |
    Parses a date string. Returns `Nothing` if the string cannot be parsed. The
    specific set of date formats known is defined below by
    `supportedDateFormats`
-}
parseDate :: String -> Maybe UTCTime
parseDate str =
    parseAll supportedDateFormats
    where
        parseAll fmts = listToMaybe (mapMaybe parse fmts)
        parse fmt = parseTime defaultTimeLocale fmt str

{- |
    The list of supported date formats.
    currently:

    > rfc822DateFormat:iso8601UtcDateFormats

    See also 'rfc822DateFormat'.
-}
supportedDateFormats :: [String]
supportedDateFormats =
    rfc822DateFormat:iso8601UtcDateFormats ++ [twitterDateFormat]


iso8601UtcDateFormats :: [String]
iso8601UtcDateFormats =
  [
      "%Y-%m-%dT%H:%M:%S%Q%Z", -- 2014-07-18T12:01:57-0800
      "%Y-%m-%dT%H:%M:%S%Q",
      "%Y-%m-%dT%H:%M",
      "%Y-%m-%dT%H",
      "%Y-%m-%d",
      "%Y-%m",
      "%Y"
  ]

twitterDateFormat :: String
twitterDateFormat = "%a %b %d %T %Z %Y"

-- Private Types --------------------------------------------------------------
-- Private Functions ----------------------------------------------------------


