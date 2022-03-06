{-# OPTIONS_GHC -cpp -pgmP "cpphs --layout --hashes --cpp" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

--
-- Copyright (c) 2005-2022  Stefan Wehr - http://www.stefanwehr.de
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

-}

module Test.Framework.HUnitWrapper (

  -- * Assertions on Bool values
  assertBool, assertBoolVerbose,

  -- * Equality assertions
  assertEqual, assertEqualVerbose,
  assertEqualPretty, assertEqualPrettyVerbose,
  assertEqualNoShow, assertEqualNoShowVerbose,

  -- * Inequality assertions
  assertNotEqual, assertNotEqualVerbose,
  assertNotEqualPretty, assertNotEqualPrettyVerbose,
  assertNotEqualNoShow, assertNotEqualNoShowVerbose,

  -- * Assertions on lists
  assertListsEqualAsSets, assertListsEqualAsSetsVerbose,
  assertNotEmpty, assertNotEmptyVerbose,
  assertEmpty, assertEmptyVerbose,
  assertElem, assertElemVerbose,

  -- * Assertions for exceptions
  assertThrows, assertThrowsVerbose,
  assertThrowsSome, assertThrowsSomeVerbose,
  assertThrowsIO, assertThrowsIOVerbose,
  assertThrowsSomeIO, assertThrowsSomeIOVerbose,
  assertThrowsM, assertThrowsMVerbose,
  assertThrowsSomeM, assertThrowsSomeMVerbose,

  -- * Assertions on Either values
  assertLeft, assertLeftVerbose,
  assertLeftNoShow, assertLeftNoShowVerbose,
  assertRight, assertRightVerbose,
  assertRightNoShow, assertRightNoShowVerbose,

  -- * Assertions on Just values
  assertJust, assertJustVerbose,
  assertNothing, assertNothingVerbose,
  assertNothingNoShow, assertNothingNoShowVerbose,

  -- * General failure
  assertFailure,

  -- * Pending unit tests
  unitTestPending, unitTestPending',

  -- * Sub assertions
  subAssert, subAssertVerbose,

  -- * Generalized assertions and failures in AssertM
  {- |
       The following definitions generalize the the monad in which assertions are executed.
       Usually, assertions are executed in the @IO@ monad. The @AssertM@ monad
       (see "Test.Framework.AssertM") allows you to evaluate assertions also as pure functions.
   -}
  -- ** Assertions on Bool values
  gassertBool, gassertBoolVerbose,

  -- ** Equality assertions
  gassertEqual, gassertEqualVerbose,
  gassertEqualPretty, gassertEqualPrettyVerbose,
  gassertEqualNoShow, gassertEqualNoShowVerbose,

  -- ** Inequality assertions
  gassertNotEqual, gassertNotEqualVerbose,
  gassertNotEqualPretty, gassertNotEqualPrettyVerbose,
  gassertNotEqualNoShow, gassertNotEqualNoShowVerbose,

  -- ** Assertions on lists
  gassertListsEqualAsSets, gassertListsEqualAsSetsVerbose,
  gassertNotEmpty, gassertNotEmptyVerbose,
  gassertEmpty, gassertEmptyVerbose,
  gassertElem, gassertElemVerbose,

  -- ** Assertions on Either values
  gassertLeft, gassertLeftVerbose,
  gassertLeftNoShow, gassertLeftNoShowVerbose,
  gassertRight, gassertRightVerbose,
  gassertRightNoShow, gassertRightNoShowVerbose,

  -- ** Assertions on Just values
  gassertJust, gassertJustVerbose,
  gassertNothing, gassertNothingVerbose,
  gassertNothingNoShow, gassertNothingNoShowVerbose,

  -- ** General failure
  gassertFailure,

  -- ** Sub assertions
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
Fail with the given reason in some 'AssertM' monad.
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

assertBool_ :: (HasCallStack, AssertM m) => String -> String -> Bool -> m ()
assertBool_ name s False = genericAssertFailure (mkMsg name s "failed")
assertBool_ _ _ True = return ()

-- | Fail if the 'Bool' value is 'False'.
assertBool :: HasCallStack => Bool -> IO ()
assertBool = assertBool_ "assertBool" ""

assertBoolVerbose :: HasCallStack => String -> Bool -> IO ()
assertBoolVerbose = assertBool_ "assertBoolVerbose"

gassertBool :: (HasCallStack, AssertM m) => Bool -> m ()
gassertBool = assertBool_ "gassertBool" ""

gassertBoolVerbose :: (HasCallStack, AssertM m) => String -> Bool -> m ()
gassertBoolVerbose = assertBool_ "gassertBoolVerbose"

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

assertEqual_ :: (Eq a, Show a, AssertM m, HasCallStack)
                 => String -> String -> a -> a -> m ()
assertEqual_ name s expected actual =
    if expected /= actual
       then do let x = equalityFailedMessage expected actual
               genericAssertFailure (mkColorMsg name s $
                                      noColor failedAt +++ x)
       else return ()

-- | Fail in some 'AssertM' monad if the two values of type @a@ are not equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
gassertEqualVerbose :: (Eq a, Show a, AssertM m, HasCallStack) => String -> a -> a -> m ()
gassertEqualVerbose = assertEqual_ "gassertEqualVerbose"

-- | Fail in some 'AssertM' monad if the two values of type @a@ are not equal.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
gassertEqual :: (Eq a, Show a, AssertM m, HasCallStack) => a -> a -> m ()
gassertEqual = assertEqual_ "gassertEqual" ""

-- | Fail if the two values of type @a@ are not equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
assertEqualVerbose :: (Eq a, Show a, HasCallStack) => String -> a -> a -> IO ()
assertEqualVerbose = assertEqual_ "assertEqualVerbose"

-- | Fail if the two values of type @a@ are not equal.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
assertEqual :: (Eq a, Show a, HasCallStack) => a -> a -> IO ()
assertEqual = assertEqual_ "assertEqual" ""

assertNotEqual_ :: (Eq a, Show a, AssertM m, HasCallStack)
                => String -> String -> a -> a -> m ()
assertNotEqual_ name s expected actual =
    if expected == actual
       then do let x = notEqualityFailedMessage expected
               genericAssertFailure (mkMsg name s $ failedAt ++ x)
       else return ()

-- | Fail in some 'AssertM' monad if the two values of type @a@ are equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
gassertNotEqualVerbose :: (Eq a, Show a, AssertM m, HasCallStack) => String -> a -> a -> m ()
gassertNotEqualVerbose = assertNotEqual_ "gassertNotEqualVerbose"

-- | Fail in some 'AssertM' monad if the two values of type @a@ are equal.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
gassertNotEqual :: (Eq a, Show a, AssertM m, HasCallStack) => a -> a -> m ()
gassertNotEqual = assertNotEqual_ "gassertNotEqual" ""

-- | Fail if the two values of type @a@ are equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
assertNotEqualVerbose :: (Eq a, Show a, HasCallStack) => String -> a -> a -> IO ()
assertNotEqualVerbose = assertNotEqual_ "assertNotEqualVerbose"

-- | Fail if the two values of type @a@ are equal.
-- Use if @a@ is an instance of 'Show' but not of 'Pretty'.
assertNotEqual :: (Eq a, Show a, HasCallStack) => a -> a -> IO ()
assertNotEqual = assertNotEqual_ "assertNotEqual" ""

assertEqualPretty_ :: (Eq a, Pretty a, AssertM m, HasCallStack)
                   => String -> String -> a -> a -> m ()
assertEqualPretty_ name s expected actual =
    if expected /= actual
       then do let x = equalityFailedMessage' (showPretty expected) (showPretty actual)
               genericAssertFailure (mkColorMsg name s
                                      (noColor failedAt +++ x))
       else return ()

-- | Fail in some 'AssertM' monad if the two values of type @a@ are not equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Pretty'.
gassertEqualPrettyVerbose :: (Eq a, Pretty a, AssertM m, HasCallStack) => String -> a -> a -> m ()
gassertEqualPrettyVerbose = assertEqualPretty_ "gassertEqualPrettyVerbose"

-- | Fail in some 'AssertM' monad if the two values of type @a@ are not equal.
-- Use if @a@ is an instance of 'Pretty'.
gassertEqualPretty :: (Eq a, Pretty a, AssertM m, HasCallStack) => a -> a -> m ()
gassertEqualPretty = assertEqualPretty_ "gassertEqualPretty" ""

-- | Fail if the two values of type @a@ are not equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Pretty'.
assertEqualPrettyVerbose :: (Eq a, Pretty a, HasCallStack) => String -> a -> a -> IO ()
assertEqualPrettyVerbose = assertEqualPretty_ "assertEqualPrettyVerbose"

-- | Fail if the two values of type @a@ are not equal.
-- Use if @a@ is an instance of 'Pretty'.
assertEqualPretty :: (Eq a, Pretty a, HasCallStack) => a -> a -> IO ()
assertEqualPretty = assertEqualPretty_ "assertEqualPretty" ""

assertNotEqualPretty_ :: (Eq a, Pretty a, AssertM m, HasCallStack)
                       => String -> String -> a -> a -> m ()
assertNotEqualPretty_ name s expected actual =
    if expected == actual
       then do let x = notEqualityFailedMessage' (showPretty expected)
               genericAssertFailure (mkMsg name s $ failedAt ++ x)
       else return ()

-- | Fail in some 'AssertM' monad if the two values of type @a@ are equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Pretty'.
gassertNotEqualPrettyVerbose :: (Eq a, Pretty a, AssertM m, HasCallStack) => String -> a -> a -> m ()
gassertNotEqualPrettyVerbose = assertNotEqualPretty_ "gassertNotEqualPrettyVerbose"

-- | Fail in some 'AssertM' monad if the two values of type @a@ are equal.
-- Use if @a@ is an instance of 'Pretty'.
gassertNotEqualPretty :: (Eq a, Pretty a, AssertM m, HasCallStack) => a -> a -> m ()
gassertNotEqualPretty = assertNotEqualPretty_ "gassertNotEqualPretty" ""

-- | Fail if the two values of type @a@ are equal, supplying
-- an additional message.
-- Use if @a@ is an instance of 'Pretty'.
assertNotEqualPrettyVerbose :: (Eq a, Pretty a, HasCallStack) => String -> a -> a -> IO ()
assertNotEqualPrettyVerbose = assertNotEqualPretty_ "assertNotEqualPrettyVerbose"

-- | Fail if the two values of type @a@ are equal.
-- Use if @a@ is an instance of 'Pretty'.
assertNotEqualPretty :: (Eq a, Pretty a, HasCallStack) => a -> a -> IO ()
assertNotEqualPretty = assertNotEqualPretty_ "assertNotEqualPretty" ""

assertEqualNoShow_ :: (Eq a, AssertM m, HasCallStack)
                    => String -> String -> a -> a -> m ()
assertEqualNoShow_ name s expected actual =
    if expected /= actual
    then genericAssertFailure (mkMsg name s failedAt)
    else return ()

-- | Fail in some 'AssertM' monad if the two values of type @a@ are not equal, supplying
-- an additional message.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
gassertEqualNoShowVerbose :: (Eq a, AssertM m, HasCallStack) => String -> a -> a -> m ()
gassertEqualNoShowVerbose = assertEqualNoShow_ "gassertEqualNoShowVerbose"

-- | Fail in some 'AssertM' monad if the two values of type @a@ are not equal.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
gassertEqualNoShow :: (Eq a, AssertM m, HasCallStack) => a -> a -> m ()
gassertEqualNoShow = assertEqualNoShow_ "gassertEqualNoShow" ""

-- | Fail if the two values of type @a@ are not equal, supplying
-- an additional message.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
assertEqualNoShowVerbose :: (Eq a, HasCallStack) => String -> a -> a -> IO ()
assertEqualNoShowVerbose = assertEqualNoShow_ "assertEqualNoShowVerbose"

-- | Fail if the two values of type @a@ are not equal.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
assertEqualNoShow :: (Eq a, HasCallStack) => a -> a -> IO ()
assertEqualNoShow = assertEqualNoShow_ "assertEqualNoShow" ""

assertNotEqualNoShow_ :: (Eq a, AssertM m, HasCallStack)
                      => String -> String -> a -> a -> m ()
assertNotEqualNoShow_ name s expected actual =
    if expected == actual
       then genericAssertFailure (mkMsg name s failedAt)
       else return ()

-- | Fail in some 'AssertM' monad if the two values of type @a@ are equal, supplying
-- an additional message.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
gassertNotEqualNoShowVerbose :: (Eq a, AssertM m, HasCallStack) => String -> a -> a -> m ()
gassertNotEqualNoShowVerbose = assertNotEqualNoShow_ "gassertNotEqualNoShowVerbose"

-- | Fail in some 'AssertM' monad if the two values of type @a@ are equal.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
gassertNotEqualNoShow :: (Eq a, AssertM m, HasCallStack) => a -> a -> m ()
gassertNotEqualNoShow = assertNotEqualNoShow_ "gassertNotEqualNoShow" ""

-- | Fail if the two values of type @a@ are equal, supplying
-- an additional message.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
assertNotEqualNoShowVerbose :: (Eq a, HasCallStack) => String -> a -> a -> IO ()
assertNotEqualNoShowVerbose = assertNotEqualNoShow_ "assertNotEqualNoShowVerbose"

-- | Fail if the two values of type @a@ are equal.
-- Use if @a@ is neither an instance of 'Show' nor of 'Pretty'.
assertNotEqualNoShow :: (Eq a, HasCallStack) => a -> a -> IO ()
assertNotEqualNoShow = assertNotEqualNoShow_ "assertNotEqualNoShow" ""

--
-- Assertions on Lists
--

assertListsEqualAsSets_ :: (Eq a, Show a, AssertM m, HasCallStack)
                   => String -> String -> [a] -> [a] -> m ()
assertListsEqualAsSets_ name s expected actual =
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

-- | Fail in some 'AssertM' monad if the two given lists are not equal when considered as sets,
-- supplying an additional error message.
gassertListsEqualAsSetsVerbose :: (Eq a, Show a, AssertM m, HasCallStack) => String -> [a] -> [a] -> m ()
gassertListsEqualAsSetsVerbose = assertListsEqualAsSets_ "gassertListsEqualAsSetsVerbose"

-- | Fail in some 'AssertM' monad if the two given lists are not equal when considered as sets.
gassertListsEqualAsSets :: (Eq a, Show a, AssertM m, HasCallStack) => [a] -> [a] -> m ()
gassertListsEqualAsSets= assertListsEqualAsSets_ "gassertListsEqualAsSets" ""

-- | Fail if the two given lists are not equal when considered as sets,
-- supplying an additional error message.
assertListsEqualAsSetsVerbose :: (Eq a, Show a, HasCallStack) => String -> [a] -> [a] -> IO ()
assertListsEqualAsSetsVerbose = assertListsEqualAsSets_ "assertListsEqualAsSetsVerbose"

-- | Fail if the two given lists are not equal when considered as sets.
assertListsEqualAsSets :: (Eq a, Show a, HasCallStack) => [a] -> [a] -> IO ()
assertListsEqualAsSets = assertListsEqualAsSets_ "assertListsEqualAsSets" ""

assertNotEmpty_ :: (AssertM m, HasCallStack) => String -> String -> [a] -> m ()
assertNotEmpty_ name s [] =
    genericAssertFailure (mkMsg name s failedAt)
assertNotEmpty_ _ _ (_:_) = return ()

-- | Fail in some 'AssertM' monad if the given list is empty, supplying an
-- additional error message.
gassertNotEmptyVerbose :: (AssertM m, HasCallStack) => String -> [a] -> m ()
gassertNotEmptyVerbose = assertNotEmpty_ "gassertNotEmptyVerbose"

-- | Fail in some 'AssertM' monad if the given list is empty.
gassertNotEmpty :: (HasCallStack, AssertM m) => [a] -> m ()
gassertNotEmpty = assertNotEmpty_ "gassertNotEmpty" ""

-- | Fail if the given list is empty, supplying an
-- additional error message.
assertNotEmptyVerbose ::  HasCallStack => String -> [a] -> IO ()
assertNotEmptyVerbose = assertNotEmpty_ "assertNotEmptyVerbose"

-- | Fail if the given list is empty.
assertNotEmpty ::  HasCallStack => [a] -> IO ()
assertNotEmpty = assertNotEmpty_ "assertNotEmpty" ""

assertEmpty_ :: (AssertM m, HasCallStack) => String -> String -> [a] -> m ()
assertEmpty_ name s (_:_) =
    genericAssertFailure (mkMsg name s failedAt)
assertEmpty_ _ _ [] = return ()

-- | Fail in some 'AssertM' monad if the given list is not empty, supplying an
-- additional error message.
gassertEmptyVerbose :: (AssertM m, HasCallStack) => String -> [a] -> m ()
gassertEmptyVerbose = assertEmpty_ "gassertEmptyVerbose"

-- | Fail in some 'AssertM' monad if the given list is not empty.
gassertEmpty :: (HasCallStack, AssertM m) => [a] -> m ()
gassertEmpty = assertEmpty_ "gassertEmpty" ""

-- | Fail if the given list is not empty, supplying an
-- additional error message.
assertEmptyVerbose ::  HasCallStack => String -> [a] -> IO ()
assertEmptyVerbose = assertEmpty_ "assertEmptyVerbose"

-- | Fail if the given list is not empty.
assertEmpty ::  HasCallStack => [a] -> IO ()
assertEmpty = assertEmpty_ "assertEmpty" ""

assertElem_ :: (Eq a, Show a, AssertM m, HasCallStack) => String -> String -> a -> [a] -> m ()
assertElem_ name s x l =
    if x `elem` l
    then return ()
    else genericAssertFailure (mkMsg name s
                                (failedAt ++
                                  "\n element: " ++ show x ++
                                  "\n list:   " ++ show l))

-- | Fail in some 'AssertM' monad if the element given is not contained in the list, supplying
-- an additional error message.
gassertElemVerbose :: (Eq a, Show a, AssertM m, HasCallStack) => String -> a -> [a] -> m ()
gassertElemVerbose = assertElem_ "gassertElemVerbose"

-- | Fail in some 'AssertM' monad if the element given is not contained in the list.
gassertElem :: (Eq a, Show a, AssertM m, HasCallStack) => a -> [a] -> m ()
gassertElem = assertElem_ "gassertElem" ""

-- | Fail if the element given is not contained in the list, supplying
-- an additional error message.
assertElemVerbose :: (Eq a, Show a, HasCallStack) => String -> a -> [a] -> IO ()
assertElemVerbose = assertElem_ "assertElemVerbose"

-- | Fail if the element given is not contained in the list.
assertElem :: (Eq a, Show a, HasCallStack) => a -> [a] -> IO ()
assertElem = assertElem_ "assertElem" ""

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

assertLeft_ :: forall a b m . (AssertM m, Show b, HasCallStack)
             => String -> String -> Either a b -> m a
assertLeft_ _ _ (Left x) = return x
assertLeft_ name s (Right x) =
    genericAssertFailure (mkMsg name s
                           (failedAt ++
                             ": expected a Left value, given " ++
                             show (Right x :: Either b b)))

-- | Fail in some 'AssertM' monad if the given @Either a b@ value is a 'Right',
-- supplying an additional error message.
-- Use this function if @b@ is an instance of 'Show'.
gassertLeftVerbose :: (Show b, AssertM m, HasCallStack) => String -> Either a b -> m a
gassertLeftVerbose = assertLeft_ "gassertLeftVerbose"

-- | Fail in some 'AssertM' monad if the given @Either a b@ value is a 'Right'.
-- Use this function if @b@ is an instance of 'Show'.
gassertLeft :: (Show b, AssertM m, HasCallStack) => Either a b -> m a
gassertLeft = assertLeft_ "gassertLeft" ""

-- | Fail if the given @Either a b@ value is a 'Right',
-- supplying an additional error message.
-- Use this function if @b@ is an instance of 'Show'.
assertLeftVerbose :: (Show b, HasCallStack) => String -> Either a b -> IO a
assertLeftVerbose = assertLeft_ "assertLeftVerbose"

-- | Fail if the given @Either a b@ value is a 'Right'.
-- Use this function if @b@ is an instance of 'Show'.
assertLeft :: (HasCallStack, Show b) => Either a b -> IO a
assertLeft = assertLeft_ "assertLeft" ""

assertLeftNoShow_ :: (HasCallStack, AssertM m) => String -> String -> Either a b -> m a
assertLeftNoShow_ _ _ (Left x) = return x
assertLeftNoShow_ name s (Right _) =
    genericAssertFailure (mkMsg name s
                           (failedAt ++
                             ": expected a Left value, given a Right value"))

-- | Fail in some 'AssertM' monad if the given @Either a b@ value is a 'Right',
-- supplying an additional error message.
-- Use this function if @b@ is not an instance of 'Show'.
gassertLeftNoShowVerbose :: (HasCallStack, AssertM m) => String -> Either a b -> m a
gassertLeftNoShowVerbose = assertLeftNoShow_ "gassertLeftNoShowVerbose"

-- | Fail in some 'AssertM' monad if the given @Either a b@ value is a 'Right'.
-- Use this function if @b@ is not an instance of 'Show'.
gassertLeftNoShow :: (HasCallStack, AssertM m) => Either a b -> m a
gassertLeftNoShow = assertLeftNoShow_ "gassertLeftNoShow" ""

-- | Fail if the given @Either a b@ value is a 'Right',
-- supplying an additional error message.
-- Use this function if @b@ is not an instance of 'Show'.
assertLeftNoShowVerbose ::  HasCallStack => String -> Either a b -> IO a
assertLeftNoShowVerbose = assertLeftNoShow_ "assertLeftNoShowVerbose"

-- | Fail if the given @Either a b@ value is a 'Right'.
-- Use this function if @b@ is not an instance of 'Show'.
assertLeftNoShow :: HasCallStack => Either a b -> IO a
assertLeftNoShow = assertLeftNoShow_ "assertLeftNoShow" ""

assertRight_ :: forall a b m . (AssertM m, Show a, HasCallStack)
             => String -> String -> Either a b -> m b
assertRight_ _ _ (Right x) = return x
assertRight_ name s (Left x) =
    genericAssertFailure (mkMsg name s
                           (failedAt ++
                             ": expected a Right value, given " ++
                             show (Left x :: Either a a)))

-- | Fail in some 'AssertM' monad if the given @Either a b@ value is a 'Left',
-- supplying an additional error message.
-- Use this function if @a@ is an instance of 'Show'.
gassertRightVerbose :: (Show a, AssertM m, HasCallStack) => String -> Either a b -> m b
gassertRightVerbose = assertRight_ "gassertRightVerbose"

-- | Fail in some 'AssertM' monad if the given @Either a b@ value is a 'Left'.
-- Use this function if @a@ is an instance of 'Show'.
gassertRight :: (Show a, AssertM m, HasCallStack) => Either a b -> m b
gassertRight = assertRight_ "gassertRight" ""

-- | Fail if the given @Either a b@ value is a 'Left',
-- supplying an additional error message.
-- Use this function if @a@ is an instance of 'Show'.
assertRightVerbose :: (Show a, HasCallStack) => String -> Either a b -> IO b
assertRightVerbose = assertRight_ "assertRightVerbose"

-- | Fail if the given @Either a b@ value is a 'Left'.
-- Use this function if @a@ is an instance of 'Show'.
assertRight :: (HasCallStack, Show a) => Either a b -> IO b
assertRight = assertRight_ "assertRight" ""

assertRightNoShow_ :: (HasCallStack, AssertM m) => String -> String -> Either a b -> m b
assertRightNoShow_ _ _ (Right x) = return x
assertRightNoShow_ name s (Left _) =
    genericAssertFailure (mkMsg name s
                           (failedAt ++
                             ": expected a Right value, given a Left value"))

-- | Fail in some 'AssertM' monad if the given @Either a b@ value is a 'Left',
-- supplying an additional error message.
-- Use this function if @a@ is not an instance of 'Show'.
gassertRightNoShowVerbose :: (HasCallStack, AssertM m) => String -> Either a b -> m b
gassertRightNoShowVerbose = assertRightNoShow_ "gassertRightNoShowVerbose"

-- | Fail in some 'AssertM' monad if the given @Either a b@ value is a 'Left'.
-- Use this function if @a@ is not an instance of 'Show'.
gassertRightNoShow :: (HasCallStack, AssertM m) => Either a b -> m b
gassertRightNoShow = assertRightNoShow_ "gassertRightNoShow" ""

-- | Fail if the given @Either a b@ value is a 'Left',
-- supplying an additional error message.
-- Use this function if @a@ is not an instance of 'Show'.
assertRightNoShowVerbose ::  HasCallStack => String -> Either a b -> IO b
assertRightNoShowVerbose = assertRightNoShow_ "assertRightNoShowVerbose"

-- | Fail if the given @Either a b@ value is a 'Left'.
-- Use this function if @a@ is not an instance of 'Show'.
assertRightNoShow :: HasCallStack => Either a b -> IO b
assertRightNoShow = assertRightNoShow_ "assertRightNoShow" ""

--
-- Assertions on Maybe
--

assertJust_ :: (HasCallStack, AssertM m) => String -> String -> Maybe a -> m a
assertJust_ _ _ (Just x) = return x
assertJust_ name s Nothing =
    genericAssertFailure (mkMsg name s
                           (failedAt ++
                             ": expected a Just value, given Nothing"))

-- | Fail in some 'AssertM' monad if the given value is a Nothing, supplying an additional
-- error message.
gassertJustVerbose :: (HasCallStack, AssertM m) => String -> Maybe a -> m a
gassertJustVerbose = assertJust_ "gassertJustVerbose"

-- | Fail in some 'AssertM' monad if the given value is a Nothing.
gassertJust :: (HasCallStack, AssertM m) => Maybe a -> m a
gassertJust = assertJust_ "gassertJust" ""

-- | Fail if the given value is a Nothing, supplying an additional
-- error message.
assertJustVerbose :: HasCallStack => String -> Maybe a -> IO a
assertJustVerbose = assertJust_ "assertJustVerbose"

-- | Fail if the given value is a Nothing.
assertJust :: HasCallStack => Maybe a -> IO a
assertJust = assertJust_ "assertJust" ""

assertNothing_ :: (Show a, AssertM m, HasCallStack)
                => String -> String -> Maybe a -> m ()
assertNothing_ _ _ Nothing = return ()
assertNothing_ name s jx =
    genericAssertFailure (mkMsg name s
                           (failedAt ++
                             ": expected Nothing, given " ++ show jx))

-- | Fail in some 'AssertM' monad if the given @Maybe a@ value is a 'Just', supplying an additional
-- error message.
-- Use this function if @a@ is an instance of 'Show'.
gassertNothingVerbose :: (Show a, AssertM m, HasCallStack) => String -> Maybe a -> m ()
gassertNothingVerbose = assertNothing_ "gassertNothingVerbose"

-- | Fail in some 'AssertM' monad if the given @Maybe a@ value is a 'Just'.
-- Use this function if @a@ is an instance of 'Show'.
gassertNothing :: (Show a, AssertM m, HasCallStack) => Maybe a -> m ()
gassertNothing = assertNothing_ "gassertNothing" ""

-- | Fail if the given @Maybe a@ value is a 'Just', supplying an additional
-- error message.
-- Use this function if @a@ is an instance of 'Show'.
assertNothingVerbose :: (Show a, HasCallStack) => String -> Maybe a -> IO ()
assertNothingVerbose = assertNothing_ "assertNothingVerbose"

-- | Fail if the given @Maybe a@ value is a 'Just'.
-- Use this function if @a@ is an instance of 'Show'.
assertNothing :: (HasCallStack, Show a) => Maybe a -> IO ()
assertNothing = assertNothing_ "assertNothing" ""

assertNothingNoShow_ :: (HasCallStack, AssertM m) => String -> String -> Maybe a -> m ()
assertNothingNoShow_ _ _ Nothing = return ()
assertNothingNoShow_ name s _ =
    genericAssertFailure (mkMsg name s
                           (failedAt ++
                             ": expected Nothing, given a Just value"))

-- | Fail in some 'AssertM' monad if the given @Maybe a@ value is a 'Just', supplying an additional
-- error message.
-- Use this function if @a@ is not an instance of 'Show'.
gassertNothingNoShowVerbose :: (HasCallStack, AssertM m) => String -> Maybe a -> m ()
gassertNothingNoShowVerbose = assertNothingNoShow_ "gassertNothingNoShowVerbose"

-- | Fail in some 'AssertM' monad if the given @Maybe a@ value is a 'Just'.
-- Use this function if @a@ is not an instance of 'Show'.
gassertNothingNoShow :: (HasCallStack, AssertM m) => Maybe a -> m ()
gassertNothingNoShow = assertNothingNoShow_ "gassertNothingNoShow" ""

-- | Fail if the given @Maybe a@ value is a 'Just', supplying an additional
-- error message.
-- Use this function if @a@ is not an instance of 'Show'.
assertNothingNoShowVerbose :: HasCallStack => String -> Maybe a -> IO ()
assertNothingNoShowVerbose = assertNothingNoShow_ "assertNothingNoShowVerbose"

-- | Fail if the given @Maybe a@ value is a 'Just'.
-- Use this function if @a@ is not an instance of 'Show'.
assertNothingNoShow :: HasCallStack => Maybe a -> IO ()
assertNothingNoShow = assertNothingNoShow_ "assertNothingNoShow" ""

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
