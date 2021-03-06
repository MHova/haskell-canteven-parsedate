{- |
This module wraps `Date.Time.Format` and provides a handy way to parse a date
string who's format is unknown, but which may be one of several "supported"
date formats.
-}
module Canteven.ParseDate (
  parseDate,
  supportedDateFormats,
  iso8601UtcDateFormats,
  twitterDateFormat
) where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Time.Format (rfc822DateFormat, ParseTime, parseTimeM, defaultTimeLocale)

{- |
    Parses a date string. Returns `Nothing` if the string cannot be parsed. The
    specific set of date formats known is defined below by
    `supportedDateFormats`
-}
parseDate :: (ParseTime t) => String -> Maybe t
parseDate str =
    parseAll supportedDateFormats
    where
        parseAll fmts = listToMaybe (mapMaybe parse fmts)
        parse fmt = parseTimeM True defaultTimeLocale fmt str

{- |
    The list of supported date formats.
    currently:

    > rfc822DateFormat:iso8601UtcDateFormats

    See also 'rfc822DateFormat'.
-}
supportedDateFormats :: [String]
supportedDateFormats =
    rfc822DateFormat:twitterDateFormat:epochFormat:iso8601UtcDateFormats


iso8601UtcDateFormats :: [String]
iso8601UtcDateFormats =
  [
      "%Y-%m-%dT%H:%M:%S%Q%Z", -- 2014-07-18T12:01:57-0800
      "%Y-%m-%dT%H:%M:%S%Q",
      "%Y-%m-%dT%H:%M",
      "%Y-%m-%dT%H",
      "%Y-%m-%d",
      "%Y-%m"
  ]

twitterDateFormat :: String
twitterDateFormat = "%a %b %d %T %Z %Y"

{- |
    In seconds since Jan 1, 1970
-}
epochFormat :: String
epochFormat = "%s"


