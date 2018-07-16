{-# LANGUAGE LambdaCase #-}

module Main ( main ) where

import Canteven.ParseDate (parseDate)
import Control.Exception.Safe (tryAny)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Locale (iso8601DateFormat)


main :: IO ()
main = do
  testZulu
  testFourDigitTZ
  test8601StyleTz
  testTwitterBasic
  testTwitterWithTZ
  testFracSecond
  testEpochTime


testZulu :: IO ()
testZulu =
  test
    "parsing with Zulu suffix (\"Z\")"
    ("2014-10-01T21:13:00Z" `parseEquals` "2014-10-01T21:13:00")

testFourDigitTZ :: IO ()
testFourDigitTZ =
  test
    "parsing with RFC822-style four-digit tz (\"+0400\")"
    ("2014-10-01T21:13:00+0400" `parseEquals` "2014-10-01T17:13:00")

test8601StyleTz :: IO ()
test8601StyleTz =
  test
    "parsing with ISO8601-style hh:mm tz (\"+04:00\")"
    ("2014-10-01T21:13:00+04:00" `parseEquals` "2014-10-01T17:13:00")

testFracSecond :: IO ()
testFracSecond =
  test
    "parsing fractional seconds"
    ("2014-10-01T21:13:00.000Z" `parseEquals` "2014-10-01T21:13:00")

testTwitterBasic :: IO ()
testTwitterBasic =
  test
    "parsing Twitter's basic format"
    ("Fri Aug 21 21:48:25 +0000 2009" `parseEquals` "2009-08-21T21:48:25")

testTwitterWithTZ :: IO ()
testTwitterWithTZ =
  test
    "parsing Twitter with a tz"
    ("Fri Aug 21 21:48:25 -0200 2009" `parseEquals` "2009-08-21T23:48:25")

testEpochTime :: IO ()
testEpochTime =
  test
    "parsing epoch time"
    ("1414180021" `parseEquals` "2014-10-24T19:47:01")

test :: String -> IO () -> IO ()
test name action =
  tryAny action >>= \case
    Left err ->
      fail $ "Test " ++ name ++ " failed: " ++ show err
    Right () -> putStrLn $ "Test " ++ name ++ " passed."


parseEquals :: String -> String -> IO ()
parseEquals src dest = case parseDate src of
  Nothing -> fail $ "Can't parse date from: " ++ src
  Just t ->
    case renderDate t == dest of
      False -> fail $ "parsed string " ++ show src ++ " wasn't the same as " ++ show dest ++ ": got " ++ show (renderDate t)
      True -> return ()


renderDate :: UTCTime -> String
renderDate = formatTime defaultTimeLocale $ iso8601DateFormat $ Just "%H:%M:%S"


