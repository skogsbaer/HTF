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

  extractPendingMessage,
  quickCheckTestFail, quickCheckTestError, quickCheckTestPending,
  quickCheckTestPass,
  unitTestFail, unitTestPending, blackBoxTestFail,

) where

import Test.Framework.TestTypes

import Data.List ( isPrefixOf )
import qualified Test.HUnit.Lang as HU

-- import Test.Framework.TestManager

-- A pending test case is treated as a failed testcase, but the error message
-- starts with the given prefix.
pendingPrefix :: String
pendingPrefix = "__PENDING__"

makePendingMessage :: String -> String
makePendingMessage = (++) pendingPrefix

extractPendingMessage :: String -> Maybe String
extractPendingMessage msg =
    if pendingPrefix `isPrefixOf` msg
       then Just $ drop (length pendingPrefix) msg
       else Nothing

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

unitTestFail :: String -> IO a
unitTestFail s =
    do assertFailureHTF s
       error "unitTestFail: UNREACHABLE"

-- |Mark a unit test as pending without removing it from the test suite.
unitTestPending :: String -> IO a
unitTestPending s = unitTestFail (makePendingMessage s)

blackBoxTestFail :: String -> Assertion
blackBoxTestFail = assertFailureHTF
