module Canteven.TestParseDate ( tests ) where

import Canteven.ParseDate (parseDate)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Distribution.TestSuite (Progress (..), Result (..), Test (..),
  TestInstance (..))
import System.Locale (iso8601DateFormat)

tests :: IO [Test]
tests = return [Group "timezone tests for ISO8601 format" True [testZulu, testFourDigitTZ, test8601StyleTz],
                Group "Twitter format tests" True [testTwitterBasic, testTwitterWithTZ],
                Group "fractional second tests" True [testFracSecond],
                Group "epoch time tests" True [testEpochTime]]

testZulu = Test $ defaultTest {
  run = return $ Finished $ "2014-10-01T21:13:00Z" `parseEquals` "2014-10-01T21:13:00",
  name = "parsing with Zulu suffix (\"Z\")"
  }

testFourDigitTZ = Test $ defaultTest {
  run = return $ Finished $ "2014-10-01T21:13:00+0400" `parseEquals` "2014-10-01T17:13:00",
  name = "parsing with RFC822-style four-digit tz (\"+0400\")"
  }

test8601StyleTz = Test $ defaultTest {
  run = return $ Finished $ "2014-10-01T21:13:00+04:00" `parseEquals` "2014-10-01T17:13:00",
  name = "parsing with ISO8601-style hh:mm tz (\"+04:00\")"
  }

testFracSecond = Test $ defaultTest {
  run = return $ Finished $ "2014-10-01T21:13:00.000Z" `parseEquals` "2014-10-01T21:13:00",
  name = "parsing fractional seconds"
  }

testTwitterBasic = Test $ defaultTest {
  run = return $ Finished $ "Fri Aug 21 21:48:25 +0000 2009" `parseEquals` "2009-08-21T21:48:25",
  name = "parsing Twitter's basic format"
  }

testTwitterWithTZ = Test $ defaultTest {
  run = return $ Finished $ "Fri Aug 21 21:48:25 -0200 2009" `parseEquals` "2009-08-21T23:48:25",
  name = "parsing Twitter with a tz"
  }

testEpochTime = Test $ defaultTest {
  run = return $ Finished $ "1414180021" `parseEquals` "2014-10-24T19:47:01",
  name = "parsing epoch time"
  }

defaultTest = TestInstance {
  run = return $ Finished $ Pass,
  name = "",
  tags = [],
  options = [],
  setOption = \_ _ -> Right defaultTest  -- FIXME
  }

parseEquals :: String -> String -> Result
parseEquals src dest = case parseDate src of
  Nothing -> Fail $ "couldn't parse " ++ show src
  Just t -> case renderDate t == dest of
    False -> Fail $ "parsed string " ++ show src ++ " wasn't the same as " ++ show dest ++ ": got " ++ show (renderDate t)
    True -> Pass

renderDate :: UTCTime -> String
renderDate = formatTime defaultTimeLocale $ iso8601DateFormat $ Just "%H:%M:%S"
