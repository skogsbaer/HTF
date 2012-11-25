{-# OPTIONS_GHC -cpp -pgmPcpphs -optP --layout -optP --hashes -optP --cpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  assertBool_, assertBoolVerbose_,

  -- * Equality assertions
  assertEqual_, assertEqualVerbose_,
  assertEqualPretty_, assertEqualPrettyVerbose_,
  assertEqualNoShow_, assertEqualNoShowVerbose_,
  assertNotEqual_, assertNotEqualVerbose_,
  assertNotEqualPretty_, assertNotEqualPrettyVerbose_,
  assertNotEqualNoShow_, assertNotEqualNoShowVerbose_,

  -- * Assertions on lists
  assertListsEqualAsSets_, assertListsEqualAsSetsVerbose_,
  assertNotEmpty_, assertNotEmptyVerbose_,
  assertEmpty_, assertEmptyVerbose_,

  -- * Assertions for exceptions
  assertThrows_, assertThrowsVerbose_,
  assertThrowsSome_, assertThrowsSomeVerbose_,
  assertThrowsIO_, assertThrowsIOVerbose_,
  assertThrowsSomeIO_, assertThrowsSomeIOVerbose_,

  -- * Assertions on Either values
  assertLeft_, assertLeftVerbose_,
  assertLeftNoShow_, assertLeftNoShowVerbose_,
  assertRight_, assertRightVerbose_,
  assertRightNoShow_, assertRightNoShowVerbose_,

  -- * Assertions on Just values
  assertJust_, assertJustVerbose_,
  assertNothing_, assertNothingVerbose_,
  assertNothingNoShow_, assertNothingNoShowVerbose_,

  -- * General failure
  assertFailure_,

  -- * Pending unit tests
  unitTestPending, unitTestPending'

) where

import System.IO ( stderr )
import Data.List ( (\\) )
import Control.Exception
import Control.Monad
import qualified Test.HUnit as HU hiding ( assertFailure )
import qualified Language.Haskell.Exts.Parser as HE
import qualified Language.Haskell.Exts.Pretty as HE

import Test.Framework.TestManager
import Test.Framework.TestManagerInternal
import Test.Framework.Location
import Test.Framework.Utils
import Test.Framework.Diff
import Test.Framework.Colors
import Test.Framework.Pretty

-- WARNING: do not forget to add a preprocessor macro for new assertions!!

assertFailure__ :: Location -> String -> IO a
assertFailure__ loc s = unitTestFail (Just loc) s

{- |
Fail with the given reason, supplying the error location and the error message.
-}
assertFailure_ :: Location -> String -> IO a
assertFailure_ loc s =
    assertFailure__ loc (mkMsg "assertFailure" "" ("failed at " ++ showLoc loc) ++ ": " ++ s)

{- |
Use @unitTestPending' msg test@ to mark the given test as pending
without removing it from the test suite and without deleting or commenting out the test code.
-}
unitTestPending' :: String -> IO a -> IO a
unitTestPending' msg _ = unitTestPending msg

mkMsg :: String -> String -> String -> String
mkMsg fun extraInfo s =
    if null extraInfo
       then fun ++ (' ':s)
       else fun ++ " (" ++ extraInfo ++ ") " ++ s

--
-- Dirty macro hackery (I'm too lazy ...)
--
#define CreateAssertionsGeneric(__name__, __ctx__, __type__, __ret__) \
__name__##Verbose_ :: __ctx__ Location -> String -> __type__ -> __ret__; \
__name__##Verbose_ = _##__name__##_ (#__name__ ++ "Verbose"); \
__name__##_ :: __ctx__ Location -> __type__ -> __ret__; \
__name__##_ loc = _##__name__##_ #__name__ loc ""

#define CreateAssertionsCtx(__name__, __ctx__, __type__) \
CreateAssertionsGeneric(__name__, __ctx__ =>, __type__, HU.Assertion)

#define CreateAssertions(__name__, __type__) \
CreateAssertionsGeneric(__name__, , __type__, HU.Assertion)

#define CreateAssertionsCtxRet(__name__, __ctx__, __type__, __ret__) \
CreateAssertionsGeneric(__name__, __ctx__ =>, __type__, __ret__)

#define CreateAssertionsRet(__name__, __type__, __ret__) \
CreateAssertionsGeneric(__name__, , __type__, __ret__)

#define DocAssertion(__name__, __text__) \
  {- | __text__ The 'String' parameter in the @Verbose@ \
      variant can be used to provide extra information about the error. Do not use \
      @__name__##_@ and @__name__##Verbose_@ directly, use the macros @__name__@ \
      and @__name__##Verbose@ instead. These macros, provided by the @htfpp@ preprocessor, \
      insert the 'Location' parameter automatically. -}

--
-- Boolean Assertions
--

_assertBool_ :: String -> Location -> String -> Bool -> HU.Assertion
_assertBool_ name loc s False =
    assertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc))
_assertBool_ _ _ _   True = return ()

DocAssertion(assertBool, Fail if the 'Bool' value is 'False'.)
CreateAssertions(assertBool, Bool)

--
-- Equality Assertions
--

equalityFailedMessage :: String -> String -> IO String
equalityFailedMessage exp act =
    do d <- diffWithSensibleConfig expP actP
       expected_ <- colorize firstDiffColor "* expected:"
       but_got_ <- colorize secondDiffColor "* but got:"
       diff_ <- colorize diffColor "* diff:"
       return ("\n" ++ expected_ ++ " " ++ withNewline expP ++
               "\n" ++ but_got_ ++ "  " ++ withNewline actP ++
               "\n" ++ diff_ ++ "     " ++ withNewline d ++
               (if stringEq
                   then "\nWARNING: strings are equal but actual values differ!"
                   else ""))
    where
      withNewline s =
          case lines s of
            [] -> s
            [_] -> s
            _ -> '\n':s
      (expP, actP, stringEq) =
          case (pp exp, pp act) of
            (Nothing, _) -> (exp, act, exp == act)
            (_, Nothing) -> (exp, act, exp == act)
            (Just expP, Just actP)
                | expP == actP ->
                    if exp /= act
                       then (exp, act, exp == act)
                       else (expP, actP, True)
                | otherwise -> (expP, actP, False)
      pp s =
          case HE.parseExp s of
            HE.ParseOk x -> Just $ HE.prettyPrint x
            HE.ParseFailed{} -> Nothing

notEqualityFailedMessage :: String -> IO String
notEqualityFailedMessage exp =
    do return (": Objects are equal\n" ++ pp exp)
    where
      pp s =
          case HE.parseExp s of
            HE.ParseOk x -> HE.prettyPrint x
            HE.ParseFailed{} -> s

_assertEqual_ :: (Eq a, Show a)
                 => String -> Location -> String -> a -> a -> HU.Assertion
_assertEqual_ name loc s expected actual =
    if expected /= actual
       then do x <- equalityFailedMessage (show expected) (show actual)
               assertFailure__ loc (mkMsg name s $ "failed at " ++ showLoc loc ++ x)
       else return ()

DocAssertion(assertEqual, Fail if the two values of type @a@ are not equal.
             The first parameter denotes the expected value. Use these two functions
             of @a@ is an instance of 'Show' but not of 'Pretty'.)
CreateAssertionsCtx(assertEqual, (Eq a, Show a), a -> a)

_assertNotEqual_ :: (Eq a, Show a)
                 => String -> Location -> String -> a -> a -> HU.Assertion
_assertNotEqual_ name loc s expected actual =
    if expected == actual
       then do x <- notEqualityFailedMessage (show expected)
               assertFailure__ loc (mkMsg name s $ "failed at " ++ showLoc loc ++ x)
       else return ()

DocAssertion(assertNotEqual, Fail if the two values of type @a@ are equal.
             The first parameter denotes the expected value. Use these two functions
             of @a@ is an instance of 'Show' but not of 'Pretty'.)
CreateAssertionsCtx(assertNotEqual, (Eq a, Show a), a -> a)

_assertEqualPretty_ :: (Eq a, Pretty a)
                       => String -> Location -> String -> a -> a -> HU.Assertion
_assertEqualPretty_ name loc s expected actual =
    if expected /= actual
       then do x <- equalityFailedMessage (showPretty expected) (showPretty actual)
               assertFailure__ loc (mkMsg name s $ "failed at " ++ showLoc loc ++ x)
       else return ()

DocAssertion(assertEqualPretty, Fail if the two values of type @a@ are not equal.
             The first parameter denotes the expected value. Use these two functions
             of @a@ is an instance of 'Pretty'.)
CreateAssertionsCtx(assertEqualPretty, (Eq a, Pretty a), a -> a)

_assertNotEqualPretty_ :: (Eq a, Pretty a)
                       => String -> Location -> String -> a -> a -> HU.Assertion
_assertNotEqualPretty_ name loc s expected actual =
    if expected == actual
       then do x <- notEqualityFailedMessage (showPretty expected)
               assertFailure__ loc (mkMsg name s $ "failed at " ++ showLoc loc ++ x)
       else return ()
DocAssertion(assertNotEqualPretty, Fail if the two values of type @a@ are equal.
             The first parameter denotes the expected value. Use these two functions
             of @a@ is an instance of 'Pretty'.)
CreateAssertionsCtx(assertNotEqualPretty, (Eq a, Pretty a), a -> a)

_assertEqualNoShow_ :: Eq a
                    => String -> Location -> String -> a -> a -> HU.Assertion
_assertEqualNoShow_ name loc s expected actual =
    if expected /= actual
       then assertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc))
       else return ()
DocAssertion(assertEqualNoShow, Fail if the two values of type @a@ are not equal.
             The first parameter denotes the expected value. Use these two functions
             of @a@ is neither an instance of 'Show' nor 'Pretty'. Be aware that in this
             case the generated error message might not be very helpful.)
CreateAssertionsCtx(assertEqualNoShow, Eq a, a -> a)

_assertNotEqualNoShow_ :: Eq a
                    => String -> Location -> String -> a -> a -> HU.Assertion
_assertNotEqualNoShow_ name loc s expected actual =
    if expected == actual
       then assertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc))
       else return ()
DocAssertion(assertNotEqualNoShow, Fail if the two values of type @a@ are equal.
             The first parameter denotes the expected value. Use these two functions
             of @a@ is neither an instance of 'Show' nor 'Pretty'. Be aware that in this
             case the generated error message might not be very helpful.)
CreateAssertionsCtx(assertNotEqualNoShow, Eq a, a -> a)

--
-- Assertions on Lists
--

_assertListsEqualAsSets_ :: (Eq a, Show a)
                   => String -> Location -> String -> [a] -> [a] -> HU.Assertion
_assertListsEqualAsSets_ name loc s expected actual =
    let ne = length expected
        na = length actual
        in case () of
            _| ne /= na ->
                 assertFailure__ loc (mkMsg name s
                                ("failed at " ++ showLoc loc
                                 ++ "\n expected length: " ++ show ne
                                 ++ "\n actual length: " ++ show na))
             | not (unorderedEq expected actual) ->
                 do x <- equalityFailedMessage (show expected) (show actual)
                    assertFailure__ loc (mkMsg "assertSetEqual" s
                                   ("failed at " ++ showLoc loc ++ x))
             | otherwise -> return ()
    where unorderedEq l1 l2 =
              null (l1 \\ l2) && null (l2 \\ l1)
DocAssertion(assertListsEqualAsSets, Fail if the two given lists are not equal
                                     when considered as sets. The first list parameter
                                     denotes the expected value.)
CreateAssertionsCtx(assertListsEqualAsSets, (Eq a, Show a), [a] -> [a])

_assertNotEmpty_ :: String -> Location -> String -> [a] -> HU.Assertion
_assertNotEmpty_ name loc s [] =
    assertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc))
_assertNotEmpty_ _ _ _ (_:_) = return ()
DocAssertion(assertNotEmpty, Fail if the given list is empty.)
CreateAssertions(assertNotEmpty, [a])

_assertEmpty_ :: String -> Location -> String -> [a] -> HU.Assertion
_assertEmpty_ name loc s (_:_) =
    assertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc))
_assertEmpty_ _ _ _ [] = return ()
DocAssertion(assertEmpty, Fail if the given list is a non-empty list.)
CreateAssertions(assertEmpty, [a])

--
-- Assertions for Exceptions
--

_assertThrowsIO_ :: Exception e
                 => String -> Location -> String -> IO a -> (e -> Bool) -> HU.Assertion
_assertThrowsIO_ name loc s x f =
    do res <- try x
       case res of
         Right _ -> assertFailure__ loc (mkMsg name s
                                   ("failed at " ++ showLoc loc ++
                                    ": no exception was thrown"))
         Left e -> if f e then return ()
                   else assertFailure__ loc (mkMsg name s
                                       ("failed at " ++
                                        showLoc loc ++
                                        ": wrong exception was thrown: " ++
                                        show e))
DocAssertion(assertThrowsIO, Fail if executing the 'IO' action does not
             throw an exception satisfying the given predicate @(e -> Bool)@.)
CreateAssertionsCtx(assertThrowsIO, Exception e, IO a -> (e -> Bool))

_assertThrowsSomeIO_ :: String -> Location -> String -> IO a -> HU.Assertion
_assertThrowsSomeIO_ name loc s x = _assertThrowsIO_ name loc s x (\ (e::SomeException) -> True)
DocAssertion(assertThrowsSomeIO, Fail if executing the 'IO' action does not
             throw an exception.)
CreateAssertions(assertThrowsSomeIO, IO a)

_assertThrows_ :: Exception e
               => String -> Location -> String -> a -> (e -> Bool) -> HU.Assertion
_assertThrows_ name loc s x f = _assertThrowsIO_ name loc s (evaluate x) f
DocAssertion(assertThrows, Fail if evaluating the expression of type @a@ does not
             throw an exception satisfying the given predicate @(e -> Bool)@.)
CreateAssertionsCtx(assertThrows, Exception e, a -> (e -> Bool))

_assertThrowsSome_ :: String -> Location -> String -> a -> HU.Assertion
_assertThrowsSome_ name loc s x =
    _assertThrows_ name loc s x (\ (e::SomeException) -> True)
DocAssertion(assertThrowsSome, Fail if evaluating the expression of type @a@ does not
             throw an exception.)
CreateAssertions(assertThrowsSome, a)

--
-- Assertions on Either
--

_assertLeft_ :: forall a b . Show b
             => String -> Location -> String -> Either a b -> IO a
_assertLeft_ _ _ _ (Left x) = return x
_assertLeft_ name loc s (Right x) =
    assertFailure__ loc (mkMsg name s
                   ("failed at " ++ showLoc loc ++
                    ": expected a Left value, given " ++
                    show (Right x :: Either b b)))
DocAssertion(assertLeft, Fail if the given @Either a b@ value is a 'Right'.
             Use this function if @b@ is an instance of 'Show')
CreateAssertionsCtxRet(assertLeft, Show b, Either a b, IO a)

_assertLeftNoShow_ :: String -> Location -> String -> Either a b -> IO a
_assertLeftNoShow_ _ _ _ (Left x) = return x
_assertLeftNoShow_ name loc s (Right _) =
    assertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc ++
                                 ": expected a Left value, given a Right value"))
DocAssertion(assertLeftNoShow, Fail if the given @Either a b@ value is a 'Right'.)
CreateAssertionsRet(assertLeftNoShow, Either a b, IO a)

_assertRight_ :: forall a b . Show a
              => String -> Location -> String -> Either a b -> IO b
_assertRight_ _ _ _ (Right x) = return x
_assertRight_ name loc s (Left x) =
    assertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc ++
                                 ": expected a Right value, given " ++
                                 show (Left x :: Either a a)))
DocAssertion(assertRight, Fail if the given @Either a b@ value is a 'Left'.
             Use this function if @a@ is an instance of 'Show')
CreateAssertionsCtxRet(assertRight, Show a, Either a b, IO b)

_assertRightNoShow_ :: String -> Location -> String -> Either a b -> IO b
_assertRightNoShow_ _ _ _ (Right x) = return x
_assertRightNoShow_ name loc s (Left _) =
    assertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc ++
                                 ": expected a Right value, given a Left value"))
DocAssertion(assertRightNoShow, Fail if the given @Either a b@ value is a 'Left'.)
CreateAssertionsRet(assertRightNoShow, Either a b, IO b)

--
-- Assertions on Maybe
--

_assertJust_ :: String -> Location -> String -> Maybe a -> IO a
_assertJust_ _ _ _ (Just x) = return x
_assertJust_ name loc s Nothing =
    assertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc ++
                                 ": expected a Just value, given Nothing"))
DocAssertion(assertJust, Fail is the given @Maybe a@ value is a 'Nothing'.)
CreateAssertionsRet(assertJust, Maybe a, IO a)

_assertNothing_ :: Show a
                => String -> Location -> String -> Maybe a -> HU.Assertion
_assertNothing_ _ _ _ Nothing = return ()
_assertNothing_ name loc s jx =
    assertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc ++
                                 ": expected Nothing, given " ++ show jx))
DocAssertion(assertNothing, Fail is the given @Maybe a@ value is a 'Just'.
             Use this function if @a@ is an instance of 'Show'.)
CreateAssertionsCtx(assertNothing, Show a, Maybe a)

_assertNothingNoShow_ :: String -> Location -> String -> Maybe a -> HU.Assertion
_assertNothingNoShow_ _ _ _ Nothing = return ()
_assertNothingNoShow_ name loc s _ =
    assertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc ++
                                 ": expected Nothing, given a Just value"))
DocAssertion(assertNothingNoShow, Fail is the given @Maybe a@ value is a 'Just'.)
CreateAssertions(assertNothingNoShow, Maybe a)
