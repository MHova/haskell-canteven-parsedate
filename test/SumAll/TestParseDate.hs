module SumAll.TestParseDate ( tests ) where

import Sumall.ParseDate (parseDate)
import Distribution.TestSuite (Test(..), TestInstance(..), Progress(..), Result(..))

tests :: IO [Test]
tests = return [Group "timezone tests for ISO8601 format" True [testZulu, testFourDigitTZ, test8601StyleTz],
                Group "fractional second tests" True [testFracSecond]]

testZulu = Test $ defaultTest {
  run = return $ Finished $ tryParse "2014-10-01T21:13:00Z",
  name = "parsing with Zulu suffix (\"Z\")"
  }

testFourDigitTZ = Test $ defaultTest {
  run = return $ Finished $ tryParse "2014-10-01T21:13:00+0400",
  name = "parsing with RFC822-style four-digit tz (\"+0400\")"
  }

test8601StyleTz = Test $ defaultTest {
  run = return $ Finished $ tryParse "2014-10-01T21:13:00+04:00",
  name = "parsing with ISO8601-style hh:mm tz (\"+04:00\")"
  }

testFracSecond = Test $ defaultTest {
  run = return $ Finished $ tryParse "2014-10-01T21:13:00.000Z",
  name = "parsing fractional seconds"
  }

defaultTest = TestInstance {
  run = return $ Finished $ Pass,
  name = "",
  tags = [],
  options = [],
  setOption = \_ _ -> Right defaultTest  -- FIXME
  }

tryParse :: String -> Result
tryParse s = case parseDate s of
  Just t -> Pass
  Nothing -> Fail $ "couldn't parse " ++ s
