{-# OPTIONS_GHC -cpp -pgmP "cpphs --layout --hashes --cpp" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

--
-- Copyright (c) 2005, 2009, 2012  Stefan Wehr - http://www.stefanwehr.de
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

{-|

This module provides assert-like functions for writing unit tests.

/Hint:/ Do not use the @assertXXX_@ functions
directly. Instead, for each function @assertXXX_@,
there exist a preprocessor macro @assertXXX@, which provides
the "Location" parameter automatically. Use these macros, which
are available automatically if you add

@&#x7b;-&#x23; OPTIONS_GHC -F -pgmF htfpp &#x23;-&#x7d;@

at the top of your source file (see the 'Test.Framework.Tutorial').

-}

module Test.Framework.HUnitWrapper (

  -- * Assertions on Bool values
  assertBool, assertBoolVerbose,
  gassertBool, gassertBoolVerbose,

  -- * Equality assertions
  assertEqual, assertEqualVerbose,
  gassertEqual, gassertEqualVerbose,
  assertEqualPretty, assertEqualPrettyVerbose,
  gassertEqualPretty, gassertEqualPrettyVerbose,
  assertEqualNoShow, assertEqualNoShowVerbose,
  gassertEqualNoShow, gassertEqualNoShowVerbose,
  assertNotEqual, assertNotEqualVerbose,
  gassertNotEqual, gassertNotEqualVerbose,
  assertNotEqualPretty, assertNotEqualPrettyVerbose,
  gassertNotEqualPretty, gassertNotEqualPrettyVerbose,
  assertNotEqualNoShow, assertNotEqualNoShowVerbose,
  gassertNotEqualNoShow, gassertNotEqualNoShowVerbose,

  -- * Assertions on lists
  assertListsEqualAsSets, assertListsEqualAsSetsVerbose,
  gassertListsEqualAsSets, gassertListsEqualAsSetsVerbose,
  assertNotEmpty, assertNotEmptyVerbose,
  gassertNotEmpty, gassertNotEmptyVerbose,
  assertEmpty, assertEmptyVerbose,
  gassertEmpty, gassertEmptyVerbose,
  assertElem, assertElemVerbose,
  gassertElem, gassertElemVerbose,

  -- * Assertions for exceptions
  assertThrows, assertThrowsVerbose,
  assertThrowsSome, assertThrowsSomeVerbose,
  assertThrowsIO, assertThrowsIOVerbose,
  assertThrowsSomeIO, assertThrowsSomeIOVerbose,
  assertThrowsM, assertThrowsMVerbose,
  assertThrowsSomeM, assertThrowsSomeMVerbose,

  -- * Assertions on Either values
  assertLeft_, assertLeftVerbose_,
  gassertLeft_, gassertLeftVerbose_,
  assertLeftNoShow_, assertLeftNoShowVerbose_,
  gassertLeftNoShow_, gassertLeftNoShowVerbose_,
  assertRight_, assertRightVerbose_,
  gassertRight_, gassertRightVerbose_,
  assertRightNoShow_, assertRightNoShowVerbose_,
  gassertRightNoShow_, gassertRightNoShowVerbose_,

  -- * Assertions on Just values
  assertJust_, assertJustVerbose_,
  gassertJust_, gassertJustVerbose_,
  assertNothing_, assertNothingVerbose_,
  gassertNothing_, gassertNothingVerbose_,
  assertNothingNoShow_, assertNothingNoShowVerbose_,
  gassertNothingNoShow_, gassertNothingNoShowVerbose_,

  -- * General failure
  assertFailure,
  gassertFailure,

  -- * Pending unit tests
  unitTestPending, unitTestPending',

  -- * Sub assertions
  subAssert, subAssertVerbose,
  gsubAssert, gsubAssertVerbose,

  -- * HUnit re-exports
  HU.HUnitFailure,

  -- * Tests (for internal use)
  hunitWrapperTests

) where

import Control.Exception
import qualified Control.Exception.Lifted as ExL
import Control.Monad.Trans.Control
import Control.Monad.Trans
import qualified Test.HUnit.Lang as HU
#if !MIN_VERSION_HUnit(1,4,0)
import qualified Test.HUnit.Base as HU
#endif

import GHC.Stack

import Data.List ( (\\) )
import System.IO.Unsafe (unsafePerformIO)

import Test.Framework.TestInterface
import Test.Framework.Location
import Test.Framework.Diff
import Test.Framework.Colors
import Test.Framework.Pretty
import Test.Framework.AssertM
import Test.Framework.PrettyHaskell

import qualified Data.Text as T
import qualified Data.List as List

-- WARNING: do not forget to add a preprocessor macro for new assertions!!

{- |
Fail with the given reason in some @AssertM@ monad.
-}
gassertFailure :: (HasCallStack, AssertM m) => String -> m a
gassertFailure s =
    genericAssertFailure (mkMsg "assertFailure" "" s)

-- | Specialization of 'gassertFailure' to @IO@.
assertFailure :: HasCallStack => String -> IO a
assertFailure = gassertFailure

{- |
Signals that the current unit test is pending.
-}
unitTestPending :: String -> IO a
unitTestPending s =
    failHTF (FullTestResult emptyHtfStack (Just $ noColor s) (Just Pending))

{- |
Use @unitTestPending' msg test@ to mark the given test as pending
without removing it from the test suite and without deleting or commenting out the test code.
-}
unitTestPending' :: String -> IO a -> IO a
unitTestPending' msg _ = unitTestPending msg

mkMsg :: String -> String -> String -> ColorString
mkMsg s1 s2 s3 = mkColorMsg s1 s2 (noColor s3)

mkColorMsg :: String -> String -> ColorString -> ColorString
mkColorMsg fun extraInfo s =
    let pref = if null extraInfo
               then fun ++ " "
               else fun ++ " (" ++ extraInfo ++ ") "
    in noColor pref +++ s

--
-- Boolean Assertions
--

_assertBool :: (HasCallStack, AssertM m) => String -> String -> Bool -> m ()
_assertBool name s False = genericAssertFailure (mkMsg name s "failed")
_assertBool _ _ True = return ()

-- | Fail if the 'Bool' value is 'False'.
assertBool :: HasCallStack => Bool -> IO ()
assertBool = _assertBool "assertBool" ""

assertBoolVerbose :: HasCallStack => String -> Bool -> IO ()
assertBoolVerbose = _assertBool "assertBoolVerbose"

gassertBool :: (HasCallStack, AssertM m) => Bool -> m ()
gassertBool = _assertBool "gassertBool" ""

gassertBoolVerbose :: (HasCallStack, AssertM m) => String -> Bool -> m ()
gassertBoolVerbose = _assertBool "gassertBoolVerbose"

--
-- Equality Assertions
--

equalityFailedMessage' :: String -> String -> ColorString
equalityFailedMessage' exp act =
    let !diff = unsafePerformIO (diffWithSensibleConfig exp act)
        expected_ = colorize firstDiffColor "* expected:"
        but_got_ = colorize secondDiffColor "* but got:"
        diff_ = colorize diffColor "* diff:"
    in ("\n" +++ expected_ +++ " " +++ noColor (withNewline (trim exp)) +++
        "\n" +++ but_got_ +++ "  " +++ noColor (withNewline (trim act)) +++
        "\n" +++ diff_ +++ "     " +++ newlineBeforeDiff diff +++ diff +++
        (if (exp == act)
         then "\nWARNING: strings are equal but actual values differ!"
         else ""))
    where
      withNewline s =
          case lines s of
            [] -> s
            [_] -> s
            _ -> '\n':s
      newlineBeforeDiff d =
          let f b = case colorStringFind (\c -> c == '\n') d b of
                      Just _ -> "\n"
                      Nothing -> ""
          in noColor' (f True) (f False)
      trim s =
          case List.splitAt maxLen s of
            (_, []) -> s
            (prefix, rest) ->
                prefix ++ " (removed " ++ show (length rest) ++ " trailing chars)"
      maxLen = 100000


equalityFailedMessage :: (Show a) => a -> a -> ColorString
equalityFailedMessage exp act =
    equalityFailedMessage' expP actP
    where
      (expP, actP) =
          case (prettyHaskell' exp, prettyHaskell' act) of
            (Nothing, _) -> (show exp, show act)
            (_, Nothing) -> (show exp, show act)
            (Just expP, Just actP)
                | expP == actP ->
                    (show exp, show act)
                | otherwise -> (expP, actP)

notEqualityFailedMessage :: Show a => a -> String
notEqualityFailedMessage exp =
    notEqualityFailedMessage' (prettyHaskell exp)

notEqualityFailedMessage' :: String -> String
notEqualityFailedMessage' exp =
    (": Objects are equal\n" ++ exp)

failedAt :: HasCallStack => String
failedAt =
  case failureLocation of
    Nothing -> "failed"
    Just loc -> "failed at " ++ showLoc loc

_assertEqual :: (Eq a, Show a, AssertM m, HasCallStack)
                 => String -> String -> a -> a -> m ()
_assertEqual name s expected actual =
    if expected /= actual
       then do let x = equalityFailedMessage expected actual
               genericAssertFailure (mkColorMsg name s $
                                      noColor failedAt +++ x)
       else return ()

-- | Fail in some @AssertM@ monad if the two values of type @a@ are not equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
gassertEqualVerbose :: (Eq a, Show a, AssertM m, HasCallStack) => String -> a -> a -> m ()
gassertEqualVerbose = _assertEqual "gassertEqualVerbose"

-- | Fail in some @AssertM@ monad if the two values of type @a@ are not equal.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
gassertEqual :: (Eq a, Show a, AssertM m, HasCallStack) => a -> a -> m ()
gassertEqual = _assertEqual "gassertEqual" ""

-- | Fail if the two values of type @a@ are not equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
assertEqualVerbose :: (Eq a, Show a, HasCallStack) => String -> a -> a -> IO ()
assertEqualVerbose = _assertEqual "assertEqualVerbose"

-- | Fail if the two values of type @a@ are not equal.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
assertEqual :: (Eq a, Show a, HasCallStack) => a -> a -> IO ()
assertEqual = _assertEqual "assertEqual" ""

_assertNotEqual :: (Eq a, Show a, AssertM m, HasCallStack)
                => String -> String -> a -> a -> m ()
_assertNotEqual name s expected actual =
    if expected == actual
       then do let x = notEqualityFailedMessage expected
               genericAssertFailure (mkMsg name s $ failedAt ++ x)
       else return ()

-- | Fail in some @AssertM@ monad if the two values of type @a@ are equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
gassertNotEqualVerbose :: (Eq a, Show a, AssertM m, HasCallStack) => String -> a -> a -> m ()
gassertNotEqualVerbose = _assertNotEqual "gassertNotEqualVerbose"

-- | Fail in some @AssertM@ monad if the two values of type @a@ are equal.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
gassertNotEqual :: (Eq a, Show a, AssertM m, HasCallStack) => a -> a -> m ()
gassertNotEqual = _assertNotEqual "gassertNotEqual" ""

-- | Fail if the two values of type @a@ are equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
assertNotEqualVerbose :: (Eq a, Show a, HasCallStack) => String -> a -> a -> IO ()
assertNotEqualVerbose = _assertNotEqual "assertNotEqualVerbose"

-- | Fail if the two values of type @a@ are equal.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
assertNotEqual :: (Eq a, Show a, HasCallStack) => a -> a -> IO ()
assertNotEqual = _assertNotEqual "assertNotEqual" ""

_assertEqualPretty :: (Eq a, Pretty a, AssertM m, HasCallStack)
                   => String -> String -> a -> a -> m ()
_assertEqualPretty name s expected actual =
    if expected /= actual
       then do let x = equalityFailedMessage' (showPretty expected) (showPretty actual)
               genericAssertFailure (mkColorMsg name s
                                      (noColor failedAt +++ x))
       else return ()

-- | Fail in some @AssertM@ monad if the two values of type @a@ are not equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Pretty'.
gassertEqualPrettyVerbose :: (Eq a, Pretty a, AssertM m, HasCallStack) => String -> a -> a -> m ()
gassertEqualPrettyVerbose = _assertEqualPretty "gassertEqualPrettyVerbose"

-- | Fail in some @AssertM@ monad if the two values of type @a@ are not equal.
-- Use if @a@ is an instance of 'Pretty'.
gassertEqualPretty :: (Eq a, Pretty a, AssertM m, HasCallStack) => a -> a -> m ()
gassertEqualPretty = _assertEqualPretty "gassertEqualPretty" ""

-- | Fail if the two values of type @a@ are not equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Pretty'.
assertEqualPrettyVerbose :: (Eq a, Pretty a, HasCallStack) => String -> a -> a -> IO ()
assertEqualPrettyVerbose = _assertEqualPretty "assertEqualPrettyVerbose"

-- | Fail if the two values of type @a@ are not equal.
-- Use if @a@ is an instance of 'Pretty'.
assertEqualPretty :: (Eq a, Pretty a, HasCallStack) => a -> a -> IO ()
assertEqualPretty = _assertEqualPretty "assertEqualPretty" ""

_assertNotEqualPretty :: (Eq a, Pretty a, AssertM m, HasCallStack)
                       => String -> String -> a -> a -> m ()
_assertNotEqualPretty name s expected actual =
    if expected == actual
       then do let x = notEqualityFailedMessage' (showPretty expected)
               genericAssertFailure (mkMsg name s $ failedAt ++ x)
       else return ()

-- | Fail in some @AssertM@ monad if the two values of type @a@ are equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Pretty'.
gassertNotEqualPrettyVerbose :: (Eq a, Pretty a, AssertM m, HasCallStack) => String -> a -> a -> m ()
gassertNotEqualPrettyVerbose = _assertNotEqualPretty "gassertNotEqualPrettyVerbose"

-- | Fail in some @AssertM@ monad if the two values of type @a@ are equal.
-- Use if @a@ is an instance of 'Pretty'.
gassertNotEqualPretty :: (Eq a, Pretty a, AssertM m, HasCallStack) => a -> a -> m ()
gassertNotEqualPretty = _assertNotEqualPretty "gassertNotEqualPretty" ""

-- | Fail if the two values of type @a@ are equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Pretty'.
assertNotEqualPrettyVerbose :: (Eq a, Pretty a, HasCallStack) => String -> a -> a -> IO ()
assertNotEqualPrettyVerbose = _assertNotEqualPretty "assertNotEqualPrettyVerbose"

-- | Fail if the two values of type @a@ are equal.
-- Use if @a@ is an instance of 'Pretty'.
assertNotEqualPretty :: (Eq a, Pretty a, HasCallStack) => a -> a -> IO ()
assertNotEqualPretty = _assertNotEqualPretty "assertNotEqualPretty" ""

_assertEqualNoShow :: (Eq a, AssertM m, HasCallStack)
                    => String -> String -> a -> a -> m ()
_assertEqualNoShow name s expected actual =
    if expected /= actual
    then genericAssertFailure (mkMsg name s failedAt)
    else return ()

-- | Fail in some @AssertM@ monad if the two values of type @a@ are not equal, supplying
-- an additional message.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
gassertEqualNoShowVerbose :: (Eq a, AssertM m, HasCallStack) => String -> a -> a -> m ()
gassertEqualNoShowVerbose = _assertEqualNoShow "gassertEqualNoShowVerbose"

-- | Fail in some @AssertM@ monad if the two values of type @a@ are not equal.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
gassertEqualNoShow :: (Eq a, AssertM m, HasCallStack) => a -> a -> m ()
gassertEqualNoShow = _assertEqualNoShow "gassertEqualNoShow" ""

-- | Fail if the two values of type @a@ are not equal, supplying
-- an additional message.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
assertEqualNoShowVerbose :: (Eq a, HasCallStack) => String -> a -> a -> IO ()
assertEqualNoShowVerbose = _assertEqualNoShow "assertEqualNoShowVerbose"

-- | Fail if the two values of type @a@ are not equal.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
assertEqualNoShow :: (Eq a, HasCallStack) => a -> a -> IO ()
assertEqualNoShow = _assertEqualNoShow "assertEqualNoShow" ""

_assertNotEqualNoShow :: (Eq a, AssertM m, HasCallStack)
                      => String -> String -> a -> a -> m ()
_assertNotEqualNoShow name s expected actual =
    if expected == actual
       then genericAssertFailure (mkMsg name s failedAt)
       else return ()

-- | Fail in some @AssertM@ monad if the two values of type @a@ are equal, supplying
-- an additional message.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
gassertNotEqualNoShowVerbose :: (Eq a, AssertM m, HasCallStack) => String -> a -> a -> m ()
gassertNotEqualNoShowVerbose = _assertNotEqualNoShow "gassertNotEqualNoShowVerbose"

-- | Fail in some @AssertM@ monad if the two values of type @a@ are equal.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
gassertNotEqualNoShow :: (Eq a, AssertM m, HasCallStack) => a -> a -> m ()
gassertNotEqualNoShow = _assertNotEqualNoShow "gassertNotEqualNoShow" ""

-- | Fail if the two values of type @a@ are equal, supplying
-- an additional message.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
assertNotEqualNoShowVerbose :: (Eq a, HasCallStack) => String -> a -> a -> IO ()
assertNotEqualNoShowVerbose = _assertNotEqualNoShow "assertNotEqualNoShowVerbose"

-- | Fail if the two values of type @a@ are equal.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
assertNotEqualNoShow :: (Eq a, HasCallStack) => a -> a -> IO ()
assertNotEqualNoShow = _assertNotEqualNoShow "assertNotEqualNoShow" ""

--
-- Assertions on Lists
--

_assertListsEqualAsSets :: (Eq a, Show a, AssertM m, HasCallStack)
                   => String -> String -> [a] -> [a] -> m ()
_assertListsEqualAsSets name s expected actual =
    let ne = length expected
        na = length actual
        in case () of
            _| ne /= na ->
                 do let x = equalityFailedMessage expected actual
                    genericAssertFailure (mkColorMsg name s
                                           (noColor
                                             (failedAt
                                               ++ "\n expected length: " ++ show ne
                                               ++ "\n actual length: " ++ show na) +++
                                             (if maxLength x < 5000
                                               then x else emptyColorString)))
             | not (unorderedEq expected actual) ->
                 do let x = equalityFailedMessage expected actual
                    genericAssertFailure (mkColorMsg "assertSetEqual" s
                                           (noColor failedAt +++ x))
             | otherwise -> return ()
    where unorderedEq l1 l2 =
              null (l1 \\ l2) && null (l2 \\ l1)

-- | Fail in some @AssertM@ monad if the two given lists are not equal when considered as sets,
-- supplying an additional error message.
gassertListsEqualAsSetsVerbose :: (Eq a, Show a, AssertM m, HasCallStack) => String -> [a] -> [a] -> m ()
gassertListsEqualAsSetsVerbose = _assertListsEqualAsSets "gassertListsEqualAsSetsVerbose"

-- | Fail in some @AssertM@ monad if the two given lists are not equal when considered as sets.
gassertListsEqualAsSets :: (Eq a, Show a, AssertM m, HasCallStack) => [a] -> [a] -> m ()
gassertListsEqualAsSets= _assertListsEqualAsSets "gassertListsEqualAsSets" ""

-- | Fail if the two given lists are not equal when considered as sets,
-- supplying an additional error message.
assertListsEqualAsSetsVerbose :: (Eq a, Show a, HasCallStack) => String -> [a] -> [a] -> IO ()
assertListsEqualAsSetsVerbose = _assertListsEqualAsSets "assertListsEqualAsSetsVerbose"

-- | Fail if the two given lists are not equal when considered as sets.
assertListsEqualAsSets :: (Eq a, Show a, HasCallStack) => [a] -> [a] -> IO ()
assertListsEqualAsSets = _assertListsEqualAsSets "assertListsEqualAsSets" ""

_assertNotEmpty :: (AssertM m, HasCallStack) => String -> String -> [a] -> m ()
_assertNotEmpty name s [] =
    genericAssertFailure (mkMsg name s failedAt)
_assertNotEmpty _ _ (_:_) = return ()

-- | Fail in some 'AssertM' monad if the given list is empty, supplying an
-- additional error message.
gassertNotEmptyVerbose :: (AssertM m, HasCallStack) => String -> [a] -> m ()
gassertNotEmptyVerbose = _assertNotEmpty "gassertNotEmptyVerbose"

-- | Fail in some 'AssertM' monad if the given list is empty.
gassertNotEmpty :: (HasCallStack, AssertM m) => [a] -> m ()
gassertNotEmpty = _assertNotEmpty "gassertNotEmpty" ""

-- | Fail if the given list is empty, supplying an
-- additional error message.
assertNotEmptyVerbose ::  HasCallStack => String -> [a] -> IO ()
assertNotEmptyVerbose = _assertNotEmpty "assertNotEmptyVerbose"

-- | Fail if the given list is empty.
assertNotEmpty ::  HasCallStack => [a] -> IO ()
assertNotEmpty = _assertNotEmpty "assertNotEmpty" ""

_assertEmpty :: (AssertM m, HasCallStack) => String -> String -> [a] -> m ()
_assertEmpty name s (_:_) =
    genericAssertFailure (mkMsg name s failedAt)
_assertEmpty _ _ [] = return ()

-- | Fail in some 'AssertM' monad if the given list is not empty, supplying an
-- additional error message.
gassertEmptyVerbose :: (AssertM m, HasCallStack) => String -> [a] -> m ()
gassertEmptyVerbose = _assertEmpty "gassertEmptyVerbose"

-- | Fail in some 'AssertM' monad if the given list is not empty.
gassertEmpty :: (HasCallStack, AssertM m) => [a] -> m ()
gassertEmpty = _assertEmpty "gassertEmpty" ""

-- | Fail if the given list is not empty, supplying an
-- additional error message.
assertEmptyVerbose ::  HasCallStack => String -> [a] -> IO ()
assertEmptyVerbose = _assertEmpty "assertEmptyVerbose"

-- | Fail if the given list is not empty.
assertEmpty ::  HasCallStack => [a] -> IO ()
assertEmpty = _assertEmpty "assertEmpty" ""

_assertElem :: (Eq a, Show a, AssertM m, HasCallStack) => String -> String -> a -> [a] -> m ()
_assertElem name s x l =
    if x `elem` l
    then return ()
    else genericAssertFailure (mkMsg name s
                                (failedAt ++
                                  "\n element: " ++ show x ++
                                  "\n list:   " ++ show l))

-- | Fail in some 'AssertM' monad if the element given is not contained in the list, supplying
-- an additional error message.
gassertElemVerbose :: (Eq a, Show a, AssertM m, HasCallStack) => String -> a -> [a] -> m ()
gassertElemVerbose = _assertElem "gassertElemVerbose"

-- | Fail in some 'AssertM' monad if the element given is not contained in the list.
gassertElem :: (Eq a, Show a, AssertM m, HasCallStack) => a -> [a] -> m ()
gassertElem = _assertElem "gassertElem" ""

-- | Fail if the element given is not contained in the list, supplying
-- an additional error message.
assertElemVerbose :: (Eq a, Show a, HasCallStack) => String -> a -> [a] -> IO ()
assertElemVerbose = _assertElem "assertElemVerbose"

-- | Fail if the element given is not contained in the list.
assertElem :: (Eq a, Show a, HasCallStack) => a -> [a] -> IO ()
assertElem = _assertElem "assertElem" ""

--
-- Assertions for Exceptions
--

assertThrowsIO_ :: (HasCallStack, Exception e)
                 => String -> String -> IO a -> (e -> Bool) -> IO ()
assertThrowsIO_ name s x f =
    assertThrowsM_ name s x f

-- | Fail if executing the 'IO' action does not throw an exception satisfying the given predicate
-- @(e -> Bool)@, supplying an additional error message.
assertThrowsIOVerbose :: (HasCallStack, Exception e) => String -> IO a -> (e -> Bool) -> IO ()
assertThrowsIOVerbose = assertThrowsIO_ "assertThrowsIOVerbose"

-- | Fail if executing the 'IO' action does not throw an exception satisfying the given predicate
-- @(e -> Bool)@.
assertThrowsIO :: (HasCallStack, Exception e) => IO a -> (e -> Bool) -> IO ()
assertThrowsIO = assertThrowsIO_ "assertThrowsIO" ""

assertThrowsSomeIO_ :: HasCallStack => String -> String -> IO a -> IO ()
assertThrowsSomeIO_ name s x = assertThrowsIO_ name s x (\ (_e::SomeException) -> True)

-- | Fail if executing the 'IO' action does not throw any exception,
-- supplying an additional error message.
assertThrowsSomeIOVerbose ::  HasCallStack => String -> IO a -> IO ()
assertThrowsSomeIOVerbose = assertThrowsSomeIO_ "assertThrowsSomeIOVerbose"

-- | Fail if executing the 'IO' action does not throw any exception.
assertThrowsSomeIO :: HasCallStack => IO a -> IO ()
assertThrowsSomeIO = assertThrowsSomeIO_ "assertThrowsSomeIO" ""

assertThrowsM_ :: (MonadBaseControl IO m, MonadIO m, Exception e, HasCallStack)
                => String -> String -> m a -> (e -> Bool) -> m ()
assertThrowsM_ name s x f =
    do res <- ExL.try x
       case res of
         Right _ -> liftIO $
                    genericAssertFailure (mkMsg name s
                                           (failedAt ++
                                             ": no exception was thrown"))
         Left e -> if f e then return ()
                   else liftIO $
                        genericAssertFailure (mkMsg name s
                                               (failedAt ++
                                                 ": wrong exception was thrown: " ++
                                                 show e))
-- | Fail if executing the @m@ action does not throw an exception satisfying the given predicate
-- @(e -> Bool)@, supplying an additional error message.
assertThrowsMVerbose ::
  (MonadBaseControl IO m, MonadIO m, Exception e, HasCallStack)
  => String -> m a -> (e -> Bool) -> m ()
assertThrowsMVerbose = assertThrowsM_ "assertThrowsMVerbose"

-- | Fail if executing the @m@ action does not throw an exception satisfying the given predicate
-- @(e -> Bool)@.
assertThrowsM ::
  (MonadBaseControl IO m, MonadIO m, Exception e, HasCallStack)
  => m a -> (e -> Bool) -> m ()
assertThrowsM = assertThrowsM_ "assertThrowsM" ""

assertThrowsSomeM_ :: (MonadBaseControl IO m, MonadIO m, HasCallStack)
                    => String -> String -> m a -> m ()
assertThrowsSomeM_ name s x = assertThrowsM_ name s x (\ (_e::SomeException) -> True)

-- | Fail if executing the @m@ action does not throw any exception,
-- supplying an additional error message.
assertThrowsSomeMVerbose ::
  (MonadBaseControl IO m, MonadIO m, HasCallStack)
  => String -> m a -> m ()
assertThrowsSomeMVerbose = assertThrowsSomeM_ "assertThrowsSomeMVerbose"

-- | Fail if executing the @m@ action does not throw any exception.
assertThrowsSomeM :: (MonadBaseControl IO m, MonadIO m, HasCallStack) => m a -> m ()
assertThrowsSomeM = assertThrowsSomeM_ "assertThrowsSomeM" ""

assertThrows_ :: (HasCallStack, Exception e)
               => String -> String -> a -> (e -> Bool) -> IO ()
assertThrows_ name s x f = assertThrowsIO_ name s (evaluate x) f

-- | Fail if evaluating the expression of type @a@ does not
-- throw an exception satisfying the given predicate @(e -> Bool)@,
-- supplying an additional error message.
assertThrowsVerbose :: (HasCallStack, Exception e) => String -> a -> (e -> Bool) -> IO ()
assertThrowsVerbose = assertThrows_ "assertThrowsVerbose"

-- | Fail if evaluating the expression of type @a@ does not
-- throw an exception satisfying the given predicate @(e -> Bool)@.
assertThrows :: (HasCallStack, Exception e) => a -> (e -> Bool) -> IO ()
assertThrows = assertThrows_ "assertThrows" ""

assertThrowsSome_ :: HasCallStack => String -> String -> a -> IO ()
assertThrowsSome_ name s x =
    assertThrows_ name s x (\ (_e::SomeException) -> True)

-- | Fail if evaluating the expression of type @a@ does not
-- throw any exception, supplying an additional error message.
assertThrowsSomeVerbose :: HasCallStack => String -> a -> IO ()
assertThrowsSomeVerbose = assertThrowsSome_ "assertThrowsSomeVerbose"

-- | Fail if evaluating the expression of type @a@ does not
-- throw any exception.
assertThrowsSome ::  HasCallStack => a -> IO ()
assertThrowsSome = assertThrowsSome_ "assertThrowsSome" ""

--
-- Assertions on Either
--

_assertLeft_ :: forall a b m . (AssertM m, Show b)
             => String -> Location -> String -> Either a b -> m a
_assertLeft_ _ _ _ (Left x) = return x
_assertLeft_ name loc s (Right x) =
    genericAssertFailure__ loc (mkMsg name s
                                ("failed at " ++ showLoc loc ++
                                 ": expected a Left value, given " ++
                                 show (Right x :: Either b b)))
{- | Fail if the given @Either a b@ value is a 'Right'.
             Use this function if @b@ is an instance of 'Show' The 'String' parameter in the @Verbose@       variants can be used to provide extra information about the error. The       variants @gassertLeft@ and @gassertLeftVerbose@ are generic assertions:       they run in the IO monad and can be evaluated to a 'Bool' value.       Do not use the       @assertLeft_@, @assertLeftVerbose_@, @gassertLeft_@, and @gassertLeftVerbose_@       functions directly, use the macros @assertLeft@, @assertLeftVerbose@, @gassertLeft@, and       @gassertLeftVerbose@ instead. These macros, provided by the @htfpp@ preprocessor,       insert the 'Location' parameter automatically. -}
gassertLeftVerbose_ :: (Show b, AssertM m) => Location -> String -> Either a b -> m a; gassertLeftVerbose_ = _assertLeft_ ("assertLeft" ++ "Verbose"); gassertLeft_ :: (Show b, AssertM m) => Location -> Either a b -> m a; gassertLeft_ loc = _assertLeft_ "assertLeft" loc ""; assertLeftVerbose_ :: Show b => Location -> String -> Either a b -> IO a; assertLeftVerbose_ = _assertLeft_ ("assertLeft" ++ "Verbose"); assertLeft_ :: Show b => Location -> Either a b -> IO a; assertLeft_ loc = _assertLeft_ "assertLeft" loc ""

_assertLeftNoShow_ :: AssertM m => String -> Location -> String -> Either a b -> m a
_assertLeftNoShow_ _ _ _ (Left x) = return x
_assertLeftNoShow_ name loc s (Right _) =
    genericAssertFailure__ loc (mkMsg name s
                                ("failed at " ++ showLoc loc ++
                                 ": expected a Left value, given a Right value"))
{- | Fail if the given @Either a b@ value is a 'Right'. The 'String' parameter in the @Verbose@       variants can be used to provide extra information about the error. The       variants @gassertLeftNoShow@ and @gassertLeftNoShowVerbose@ are generic assertions:       they run in the IO monad and can be evaluated to a 'Bool' value.       Do not use the       @assertLeftNoShow_@, @assertLeftNoShowVerbose_@, @gassertLeftNoShow_@, and @gassertLeftNoShowVerbose_@       functions directly, use the macros @assertLeftNoShow@, @assertLeftNoShowVerbose@, @gassertLeftNoShow@, and       @gassertLeftNoShowVerbose@ instead. These macros, provided by the @htfpp@ preprocessor,       insert the 'Location' parameter automatically. -}
gassertLeftNoShowVerbose_ :: AssertM m => Location -> String -> Either a b -> m a; gassertLeftNoShowVerbose_ = _assertLeftNoShow_ ("assertLeftNoShow" ++ "Verbose"); gassertLeftNoShow_ :: AssertM m => Location -> Either a b -> m a; gassertLeftNoShow_ loc = _assertLeftNoShow_ "assertLeftNoShow" loc ""; assertLeftNoShowVerbose_ ::  Location -> String -> Either a b -> IO a; assertLeftNoShowVerbose_ = _assertLeftNoShow_ ("assertLeftNoShow" ++ "Verbose"); assertLeftNoShow_ ::  Location -> Either a b -> IO a; assertLeftNoShow_ loc = _assertLeftNoShow_ "assertLeftNoShow" loc ""

_assertRight_ :: forall a b m . (Show a, AssertM m)
              => String -> Location -> String -> Either a b -> m b
_assertRight_ _ _ _ (Right x) = return x
_assertRight_ name loc s (Left x) =
    genericAssertFailure__ loc (mkMsg name s
                                ("failed at " ++ showLoc loc ++
                                 ": expected a Right value, given " ++
                                 show (Left x :: Either a a)))
{- | Fail if the given @Either a b@ value is a 'Left'.
             Use this function if @a@ is an instance of 'Show' The 'String' parameter in the @Verbose@       variants can be used to provide extra information about the error. The       variants @gassertRight@ and @gassertRightVerbose@ are generic assertions:       they run in the IO monad and can be evaluated to a 'Bool' value.       Do not use the       @assertRight_@, @assertRightVerbose_@, @gassertRight_@, and @gassertRightVerbose_@       functions directly, use the macros @assertRight@, @assertRightVerbose@, @gassertRight@, and       @gassertRightVerbose@ instead. These macros, provided by the @htfpp@ preprocessor,       insert the 'Location' parameter automatically. -}
gassertRightVerbose_ :: (Show a, AssertM m) => Location -> String -> Either a b -> m b; gassertRightVerbose_ = _assertRight_ ("assertRight" ++ "Verbose"); gassertRight_ :: (Show a, AssertM m) => Location -> Either a b -> m b; gassertRight_ loc = _assertRight_ "assertRight" loc ""; assertRightVerbose_ :: Show a => Location -> String -> Either a b -> IO b; assertRightVerbose_ = _assertRight_ ("assertRight" ++ "Verbose"); assertRight_ :: Show a => Location -> Either a b -> IO b; assertRight_ loc = _assertRight_ "assertRight" loc ""

_assertRightNoShow_ :: AssertM m => String -> Location -> String -> Either a b -> m b
_assertRightNoShow_ _ _ _ (Right x) = return x
_assertRightNoShow_ name loc s (Left _) =
    genericAssertFailure__ loc (mkMsg name s
                                ("failed at " ++ showLoc loc ++
                                 ": expected a Right value, given a Left value"))
{- | Fail if the given @Either a b@ value is a 'Left'. The 'String' parameter in the @Verbose@       variants can be used to provide extra information about the error. The       variants @gassertRightNoShow@ and @gassertRightNoShowVerbose@ are generic assertions:       they run in the IO monad and can be evaluated to a 'Bool' value.       Do not use the       @assertRightNoShow_@, @assertRightNoShowVerbose_@, @gassertRightNoShow_@, and @gassertRightNoShowVerbose_@       functions directly, use the macros @assertRightNoShow@, @assertRightNoShowVerbose@, @gassertRightNoShow@, and       @gassertRightNoShowVerbose@ instead. These macros, provided by the @htfpp@ preprocessor,       insert the 'Location' parameter automatically. -}
gassertRightNoShowVerbose_ :: AssertM m => Location -> String -> Either a b -> m b; gassertRightNoShowVerbose_ = _assertRightNoShow_ ("assertRightNoShow" ++ "Verbose"); gassertRightNoShow_ :: AssertM m => Location -> Either a b -> m b; gassertRightNoShow_ loc = _assertRightNoShow_ "assertRightNoShow" loc ""; assertRightNoShowVerbose_ ::  Location -> String -> Either a b -> IO b; assertRightNoShowVerbose_ = _assertRightNoShow_ ("assertRightNoShow" ++ "Verbose"); assertRightNoShow_ ::  Location -> Either a b -> IO b; assertRightNoShow_ loc = _assertRightNoShow_ "assertRightNoShow" loc ""

--
-- Assertions on Maybe
--

_assertJust_ :: AssertM m => String -> Location -> String -> Maybe a -> m a
_assertJust_ _ _ _ (Just x) = return x
_assertJust_ name loc s Nothing =
    genericAssertFailure__ loc (mkMsg name s
                                ("failed at " ++ showLoc loc ++
                                 ": expected a Just value, given Nothing"))
{- | Fail is the given @Maybe a@ value is a 'Nothing'. The 'String' parameter in the @Verbose@       variants can be used to provide extra information about the error. The       variants @gassertJust@ and @gassertJustVerbose@ are generic assertions:       they run in the IO monad and can be evaluated to a 'Bool' value.       Do not use the       @assertJust_@, @assertJustVerbose_@, @gassertJust_@, and @gassertJustVerbose_@       functions directly, use the macros @assertJust@, @assertJustVerbose@, @gassertJust@, and       @gassertJustVerbose@ instead. These macros, provided by the @htfpp@ preprocessor,       insert the 'Location' parameter automatically. -}
gassertJustVerbose_ :: AssertM m => Location -> String -> Maybe a -> m a; gassertJustVerbose_ = _assertJust_ ("assertJust" ++ "Verbose"); gassertJust_ :: AssertM m => Location -> Maybe a -> m a; gassertJust_ loc = _assertJust_ "assertJust" loc ""; assertJustVerbose_ ::  Location -> String -> Maybe a -> IO a; assertJustVerbose_ = _assertJust_ ("assertJust" ++ "Verbose"); assertJust_ ::  Location -> Maybe a -> IO a; assertJust_ loc = _assertJust_ "assertJust" loc ""

_assertNothing_ :: (Show a, AssertM m)
                => String -> Location -> String -> Maybe a -> m ()
_assertNothing_ _ _ _ Nothing = return ()
_assertNothing_ name loc s jx =
    genericAssertFailure__ loc (mkMsg name s
                                ("failed at " ++ showLoc loc ++
                                 ": expected Nothing, given " ++ show jx))
{- | Fail is the given @Maybe a@ value is a 'Just'.
             Use this function if @a@ is an instance of 'Show'. The 'String' parameter in the @Verbose@       variants can be used to provide extra information about the error. The       variants @gassertNothing@ and @gassertNothingVerbose@ are generic assertions:       they run in the IO monad and can be evaluated to a 'Bool' value.       Do not use the       @assertNothing_@, @assertNothingVerbose_@, @gassertNothing_@, and @gassertNothingVerbose_@       functions directly, use the macros @assertNothing@, @assertNothingVerbose@, @gassertNothing@, and       @gassertNothingVerbose@ instead. These macros, provided by the @htfpp@ preprocessor,       insert the 'Location' parameter automatically. -}
gassertNothingVerbose_ :: (Show a, AssertM m) => Location -> String -> Maybe a -> m (); gassertNothingVerbose_ = _assertNothing_ ("assertNothing" ++ "Verbose"); gassertNothing_ :: (Show a, AssertM m) => Location -> Maybe a -> m (); gassertNothing_ loc = _assertNothing_ "assertNothing" loc ""; assertNothingVerbose_ :: Show a => Location -> String -> Maybe a -> IO (); assertNothingVerbose_ = _assertNothing_ ("assertNothing" ++ "Verbose"); assertNothing_ :: Show a => Location -> Maybe a -> IO (); assertNothing_ loc = _assertNothing_ "assertNothing" loc ""

_assertNothingNoShow_ :: AssertM m => String -> Location -> String -> Maybe a -> m ()
_assertNothingNoShow_ _ _ _ Nothing = return ()
_assertNothingNoShow_ name loc s _ =
    genericAssertFailure__ loc (mkMsg name s
                                ("failed at " ++ showLoc loc ++
                                 ": expected Nothing, given a Just value"))
{- | Fail is the given @Maybe a@ value is a 'Just'. The 'String' parameter in the @Verbose@       variants can be used to provide extra information about the error. The       variants @gassertNothingNoShow@ and @gassertNothingNoShowVerbose@ are generic assertions:       they run in the IO monad and can be evaluated to a 'Bool' value.       Do not use the       @assertNothingNoShow_@, @assertNothingNoShowVerbose_@, @gassertNothingNoShow_@, and @gassertNothingNoShowVerbose_@       functions directly, use the macros @assertNothingNoShow@, @assertNothingNoShowVerbose@, @gassertNothingNoShow@, and       @gassertNothingNoShowVerbose@ instead. These macros, provided by the @htfpp@ preprocessor,       insert the 'Location' parameter automatically. -}
gassertNothingNoShowVerbose_ :: AssertM m => Location -> String -> Maybe a -> m (); gassertNothingNoShowVerbose_ = _assertNothingNoShow_ ("assertNothingNoShow" ++ "Verbose"); gassertNothingNoShow_ :: AssertM m => Location -> Maybe a -> m (); gassertNothingNoShow_ loc = _assertNothingNoShow_ "assertNothingNoShow" loc ""; assertNothingNoShowVerbose_ ::  Location -> String -> Maybe a -> IO (); assertNothingNoShowVerbose_ = _assertNothingNoShow_ ("assertNothingNoShow" ++ "Verbose"); assertNothingNoShow_ ::  Location -> Maybe a -> IO (); assertNothingNoShow_ loc = _assertNothingNoShow_ "assertNothingNoShow" loc ""

--
-- Sub assertions
--

-- | Use 'subAssert' if you want location information for the call site but the function
--   being called does not carry a 'HasCallStack' constraint.
subAssert :: (HasCallStack, MonadBaseControl IO m) => m a -> m a
subAssert = subAssertHTF Nothing

gsubAssert :: (HasCallStack, AssertM m) => m a -> m a
gsubAssert = genericSubAssert Nothing

subAssertVerbose :: (HasCallStack, MonadBaseControl IO m) => String -> m a -> m a
subAssertVerbose msg = subAssertHTF (Just msg)

gsubAssertVerbose :: (HasCallStack, AssertM m) => String -> m a -> m a
gsubAssertVerbose msg = genericSubAssert (Just msg)

testEqualityFailedMessage1 :: IO ()
testEqualityFailedMessage1 =
    let msg = T.unpack $ renderColorString (equalityFailedMessage [1,2,3] [1,2,3,4]) False
    in HU.assertEqual "error" msg exp
    where
      exp = "\n* expected: [1, 2, 3]\n* but got:  [1, 2, 3, 4]\n* " ++
            "diff:     \nC <...[1, 2, 3...>C \nS , 4\nC ]<......>C "

testEqualityFailedMessage2 :: IO ()
testEqualityFailedMessage2 =
    let msg = T.unpack $ renderColorString (equalityFailedMessage [1,2,3] [1,2,3]) False
    in HU.assertEqual "error" msg exp
    where
      exp = "\n* expected: [1,2,3]\n* but got:  [1,2,3]\n* " ++
            "diff:     \nWARNING: strings are equal but actual values differ!"

hunitWrapperTests =
    [("testEqualityFailedMessage1", testEqualityFailedMessage1)
    ,("testEqualityFailedMessage2", testEqualityFailedMessage2)]
