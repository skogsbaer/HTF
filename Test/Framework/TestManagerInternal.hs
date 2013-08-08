{-# LANGUAGE FlexibleContexts #-}
--
-- Copyright (c) 2009-2012   Stefan Wehr - http://www.stefanwehr.de
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA
--

module Test.Framework.TestManagerInternal (

  UnitTestResult(..),

  quickCheckTestFail, quickCheckTestError, quickCheckTestPending,
  quickCheckTestPass, deserializeQuickCheckMsg,
  unitTestFail, unitTestPending, deserializeHUnitMsg, unitTestSubAssert,
  blackBoxTestFail,

) where

import Test.Framework.TestTypes
import Test.Framework.Utils
import Test.Framework.Location
import Test.Framework.Colors

import qualified Test.HUnit.Lang as HU
import Control.Monad.Trans.Control
import qualified Control.Exception.Lifted as Exc

assertFailureHTF :: String -> Assertion
-- Important: force the string argument, otherwise an error embedded
-- lazily inside the string might escape.
assertFailureHTF s = length s `seq` HU.assertFailure s

-- This is a HACK: we encode a custom error message for QuickCheck
-- failures and errors in a string, which is later parsed using read!
quickCheckTestError :: Maybe String -> Assertion
quickCheckTestError m = assertFailureHTF (show (Error, m))

quickCheckTestFail :: Maybe String -> Assertion
quickCheckTestFail m = assertFailureHTF (show (Fail, m))

quickCheckTestPending :: String -> Assertion
quickCheckTestPending m = assertFailureHTF (show (Pending, Just m))

quickCheckTestPass :: String -> Assertion
quickCheckTestPass m = assertFailureHTF (show (Pass, Just m))

deserializeQuickCheckMsg :: String -> (TestResult, String)
deserializeQuickCheckMsg msg =
    case readM msg of
      Nothing ->
          (Error, msg)
      Just (r, ms) ->
          case ms of
            Nothing -> (r, "")
            Just s -> (r, s)

-- This is a HACK: we encode location and pending information as a datatype
-- that we show and parse later using read.
data UnitTestResult
    = UnitTestResult
      { utr_location :: Maybe Location
      , utr_callingLocations :: [(Maybe String, Location)]
      , utr_message :: ColorString
      , utr_pending :: Bool
      } deriving (Eq, Show, Read)

unitTestFail :: Maybe Location -> ColorString -> IO a
unitTestFail loc s =
    do assertFailureHTF (show (UnitTestResult loc [] s False))
       error "unitTestFail: UNREACHABLE"

unitTestSubAssert :: MonadBaseControl IO m => Location -> Maybe String -> m a -> m a
unitTestSubAssert loc mMsg action =
    action `Exc.catch` (\(HU.HUnitFailure s) -> let res = deserializeHUnitMsg s
                                                    newRes = res { utr_callingLocations = (mMsg, loc) : utr_callingLocations res }
                                                in Exc.throwIO (HU.HUnitFailure $ show newRes))

-- Mark a unit test as pending without removing it from the test suite.
unitTestPending :: String -> IO a
unitTestPending s =
    do assertFailureHTF (show (UnitTestResult Nothing [] (noColor s) True))
       error "unitTestFail: UNREACHABLE"

deserializeHUnitMsg :: String -> UnitTestResult
deserializeHUnitMsg msg =
    case readM msg of
      Just r -> r
      _ -> UnitTestResult Nothing [] (noColor msg) False

blackBoxTestFail :: String -> Assertion
blackBoxTestFail = assertFailureHTF
