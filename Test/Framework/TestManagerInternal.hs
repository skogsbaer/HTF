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
  unitTestFail, unitTestPending, deserializeHUnitMsg,
  blackBoxTestFail,

) where

import Test.Framework.TestTypes
import Test.Framework.Utils
import Test.Framework.Location

import Data.List ( isPrefixOf )
import qualified Test.HUnit.Lang as HU

-- import Test.Framework.TestManager

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
          error ("INTERNAL HTF ERROR: " ++
                 "Cannot deserialize QuickCheck " ++
                 "error message.\n[BEGIN]\n" ++
                 show msg ++ "\n[END]\n")
      Just (r, ms) ->
          case ms of
            Nothing -> (r, "")
            Just s -> (r, s)

-- This is a HACK: we encode location and pending information as a datatype
-- that we show and parse later using read.
data UnitTestResult
    = UnitTestResult
      { utr_location :: Maybe Location
      , utr_message :: String
      , utr_pending :: Bool
      } deriving (Eq, Show, Read)

unitTestFail :: Maybe Location -> String -> IO a
unitTestFail loc s =
    do assertFailureHTF (show (UnitTestResult loc s False))
       error "unitTestFail: UNREACHABLE"

-- |Mark a unit test as pending without removing it from the test suite.
unitTestPending :: String -> IO a
unitTestPending s =
    do assertFailureHTF (show (UnitTestResult Nothing s True))
       error "unitTestFail: UNREACHABLE"

deserializeHUnitMsg :: String -> UnitTestResult
deserializeHUnitMsg msg =
    case readM msg of
      Just r -> r
      _ -> UnitTestResult Nothing msg False

blackBoxTestFail :: String -> Assertion
blackBoxTestFail = assertFailureHTF
