{-# OPTIONS_GHC -cpp -pgmPcpphs -optP --layout -optP --hashes -optP --cpp #-}
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
  assertBool_, assertBoolVerbose_,
  gassertBool_, gassertBoolVerbose_,

  -- * Equality assertions
  assertEqual_, assertEqualVerbose_,
  gassertEqual_, gassertEqualVerbose_,
  assertEqualPretty_, assertEqualPrettyVerbose_,
  gassertEqualPretty_, gassertEqualPrettyVerbose_,
  assertEqualNoShow_, assertEqualNoShowVerbose_,
  gassertEqualNoShow_, gassertEqualNoShowVerbose_,
  assertNotEqual_, assertNotEqualVerbose_,
  gassertNotEqual_, gassertNotEqualVerbose_,
  assertNotEqualPretty_, assertNotEqualPrettyVerbose_,
  gassertNotEqualPretty_, gassertNotEqualPrettyVerbose_,
  assertNotEqualNoShow_, assertNotEqualNoShowVerbose_,
  gassertNotEqualNoShow_, gassertNotEqualNoShowVerbose_,

  -- * Assertions on lists
  assertListsEqualAsSets_, assertListsEqualAsSetsVerbose_,
  gassertListsEqualAsSets_, gassertListsEqualAsSetsVerbose_,
  assertNotEmpty_, assertNotEmptyVerbose_,
  gassertNotEmpty_, gassertNotEmptyVerbose_,
  assertEmpty_, assertEmptyVerbose_,
  gassertEmpty_, gassertEmptyVerbose_,
  assertElem_, assertElemVerbose_,
  gassertElem_, gassertElemVerbose_,

  -- * Assertions for exceptions
  assertThrows_, assertThrowsVerbose_,
  assertThrowsSome_, assertThrowsSomeVerbose_,
  assertThrowsIO_, assertThrowsIOVerbose_,
  assertThrowsSomeIO_, assertThrowsSomeIOVerbose_,
  assertThrowsM_, assertThrowsMVerbose_,
  assertThrowsSomeM_, assertThrowsSomeMVerbose_,

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
  assertFailure_,
  gassertFailure_,

  -- * Pending unit tests
  unitTestPending, unitTestPending',

  -- * Sub assertions
  subAssert_, subAssertVerbose_,
  gsubAssert_, gsubAssertVerbose_,

  -- * HUnit re-exports
  HU.HUnitFailure

) where

import Control.Exception
import qualified Control.Exception.Lifted as ExL
import Control.Monad.Trans.Control
import Control.Monad.Trans
import qualified Test.HUnit.Lang as HU

import Data.List ( (\\) )
import System.IO.Unsafe (unsafePerformIO)

import Test.Framework.TestInterface
import Test.Framework.Location
import Test.Framework.Diff
import Test.Framework.Colors
import Test.Framework.Pretty
import Test.Framework.AssertM
import Test.Framework.PrettyHaskell

-- WARNING: do not forget to add a preprocessor macro for new assertions!!

{- |
Fail with the given reason, supplying the error location and the error message.
-}
gassertFailure_ :: AssertM m => Location -> String -> m a
gassertFailure_ loc s =
    genericAssertFailure__ loc (mkMsg "assertFailure" ""
                                ("failed at " ++ showLoc loc ++ ": " ++ s))

-- | Specialization of 'gassertFailure'.
assertFailure_ :: Location -> String -> IO a
assertFailure_ = gassertFailure_

{- |
Signals that the current unit test is pending.
-}
unitTestPending :: String -> IO a
unitTestPending s =
    failHTF (FullTestResult Nothing [] (Just $ noColor s) (Just Pending))

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
-- Dirty macro hackery (I'm too lazy ...)
--
#define CreateAssertionsGenericNoGVariant(__name__, __ctx__, __type__, __ret__) \
__name__##Verbose_ :: __ctx__ Location -> String -> __type__ -> __ret__; \
__name__##Verbose_ = _##__name__##_ (#__name__ ++ "Verbose"); \
__name__##_ :: __ctx__ Location -> __type__ -> __ret__; \
__name__##_ loc = _##__name__##_ #__name__ loc ""
#define CreateAssertionsGeneric(__name__, __ctx__, __ctx2__, __type__, __ret__) \
g##__name__##Verbose_ :: __ctx2__ Location -> String -> __type__ -> m __ret__; \
g##__name__##Verbose_ = _##__name__##_ (#__name__ ++ "Verbose"); \
g##__name__##_ :: __ctx2__ Location -> __type__ -> m __ret__; \
g##__name__##_ loc = _##__name__##_ #__name__ loc ""; \
CreateAssertionsGenericNoGVariant(__name__, __ctx__, __type__, IO __ret__)

#define CreateAssertionsCtx(__name__, __ctx__, __ctx2__, __type__) \
CreateAssertionsGeneric(__name__, __ctx__ =>, __ctx2__ =>, __type__, ())
#define CreateAssertionsCtxNoGVariant(__name__, __ctx__, __type__) \
CreateAssertionsGenericNoGVariant(__name__, __ctx__ =>, __type__, IO ())

#define CreateAssertions(__name__, __type__) \
CreateAssertionsGeneric(__name__, , AssertM m =>, __type__, ())
#define CreateAssertionsNoGVariant(__name__, __type__) \
CreateAssertionsGenericNoGVariant(__name__, , __type__, IO ())

#define CreateAssertionsCtxRet(__name__, __ctx__, __ctx2__, __type__, __ret__) \
CreateAssertionsGeneric(__name__, __ctx__ =>, __ctx2__ =>, __type__, __ret__)
#define CreateAssertionsCtxRetNoGVariant(__name__, __ctx__, __type__, __ret__) \
CreateAssertionsGenericNoGVariant(__name__, __ctx__ =>, __type__, IO __ret__)

#define CreateAssertionsRet(__name__, __type__, __ret__) \
CreateAssertionsGeneric(__name__, , AssertM m =>, __type__, __ret__)
#define CreateAssertionsRetNoGVariant(__name__, __type__, __ret__) \
CreateAssertionsGenericNoGVariant(__name__, , __type__, IO __ret__)

#define DocAssertion(__name__, __text__) \
  {- | __text__ The 'String' parameter in the @Verbose@ \
      variants can be used to provide extra information about the error. The \
      variants @g##__name__@ and @g##__name__##Verbose@ are generic assertions: \
      they run in the IO monad and can be evaluated to a 'Bool' value. \
      Do not use the \
      @__name__##_@, @__name__##Verbose_@, @g##__name__##_@, and @g##__name__##Verbose_@ \
      functions directly, use the macros @__name__@, @__name__##Verbose@, @g##__name__@, and \
      @g##__name__##Verbose@ instead. These macros, provided by the @htfpp@ preprocessor, \
      insert the 'Location' parameter automatically. -}
#define DocAssertionNoGVariant(__name__, __text__) \
  {- | __text__ The 'String' parameter in the @Verbose@ \
      variant can be used to provide extra information about the error. \
      Do not use the \
      @__name__##_@ and @__name__##Verbose_@ \
      functions directly, use the macros @__name__@ and @__name__##Verbose@ \
      instead. These macros, provided by the @htfpp@ preprocessor, \
      insert the 'Location' parameter automatically. -}
--
-- Boolean Assertions
--

_assertBool_ :: AssertM m => String -> Location -> String -> Bool -> m ()
_assertBool_ name loc s False =
    genericAssertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc))
_assertBool_ _ _ _   True = return ()

DocAssertion(assertBool, Fail if the 'Bool' value is 'False'.)
CreateAssertions(assertBool, Bool)

--
-- Equality Assertions
--

equalityFailedMessage :: String -> String -> ColorString
equalityFailedMessage exp act =
    let !diff = unsafePerformIO (diffWithSensibleConfig expP actP)
        expected_ = colorize firstDiffColor "* expected:"
        but_got_ = colorize secondDiffColor "* but got:"
        diff_ = colorize diffColor "* diff:"
    in ("\n" +++ expected_ +++ " " +++ noColor (withNewline expP) +++
        "\n" +++ but_got_ +++ "  " +++ noColor (withNewline actP) +++
        "\n" +++ diff_ +++ "     " +++ newlineBeforeDiff diff +++ diff +++
        (if stringEq
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
      (expP, actP, stringEq) =
          case (prettyHaskell' exp, prettyHaskell' act) of
            (Nothing, _) -> (exp, act, exp == act)
            (_, Nothing) -> (exp, act, exp == act)
            (Just expP, Just actP)
                | expP == actP ->
                    if exp /= act
                       then (exp, act, exp == act)
                       else (expP, actP, True)
                | otherwise -> (expP, actP, False)

notEqualityFailedMessage :: String -> String
notEqualityFailedMessage exp =
    (": Objects are equal\n" ++ prettyHaskell exp)

_assertEqual_ :: (Eq a, Show a, AssertM m)
                 => String -> Location -> String -> a -> a -> m ()
_assertEqual_ name loc s expected actual =
    if expected /= actual
       then do let x = equalityFailedMessage (show expected) (show actual)
               genericAssertFailure__ loc (mkColorMsg name s $
                                           noColor ("failed at " ++ showLoc loc) +++ x)
       else return ()

DocAssertion(assertEqual, Fail if the two values of type @a@ are not equal.
             The first parameter denotes the expected value. Use these two functions
             of @a@ is an instance of 'Show' but not of 'Pretty'.)
CreateAssertionsCtx(assertEqual, (Eq a, Show a), (Eq a, Show a, AssertM m), a -> a)

_assertNotEqual_ :: (Eq a, Show a, AssertM m)
                 => String -> Location -> String -> a -> a -> m ()
_assertNotEqual_ name loc s expected actual =
    if expected == actual
       then do let x = notEqualityFailedMessage (show expected)
               genericAssertFailure__ loc (mkMsg name s $ "failed at " ++ showLoc loc ++ x)
       else return ()

DocAssertion(assertNotEqual, Fail if the two values of type @a@ are equal.
             The first parameter denotes the expected value. Use these two functions
             of @a@ is an instance of 'Show' but not of 'Pretty'.)
CreateAssertionsCtx(assertNotEqual, (Eq a, Show a), (Eq a, Show a, AssertM m), a -> a)

_assertEqualPretty_ :: (Eq a, Pretty a, AssertM m)
                       => String -> Location -> String -> a -> a -> m ()
_assertEqualPretty_ name loc s expected actual =
    if expected /= actual
       then do let x = equalityFailedMessage (showPretty expected) (showPretty actual)
               genericAssertFailure__ loc (mkColorMsg name s
                                           (noColor ("failed at " ++ showLoc loc) +++ x))
       else return ()

DocAssertion(assertEqualPretty, Fail if the two values of type @a@ are not equal.
             The first parameter denotes the expected value. Use these two functions
             of @a@ is an instance of 'Pretty'.)
CreateAssertionsCtx(assertEqualPretty, (Eq a, Pretty a), (Eq a, Pretty a, AssertM m), a -> a)

_assertNotEqualPretty_ :: (Eq a, Pretty a, AssertM m)
                       => String -> Location -> String -> a -> a -> m ()
_assertNotEqualPretty_ name loc s expected actual =
    if expected == actual
       then do let x = notEqualityFailedMessage (showPretty expected)
               genericAssertFailure__ loc (mkMsg name s $ "failed at " ++ showLoc loc ++ x)
       else return ()
DocAssertion(assertNotEqualPretty, Fail if the two values of type @a@ are equal.
             The first parameter denotes the expected value. Use these two functions
             of @a@ is an instance of 'Pretty'.)
CreateAssertionsCtx(assertNotEqualPretty, (Eq a, Pretty a), (Eq a, Pretty a, AssertM m), a -> a)

_assertEqualNoShow_ :: (Eq a, AssertM m)
                    => String -> Location -> String -> a -> a -> m ()
_assertEqualNoShow_ name loc s expected actual =
    if expected /= actual
    then genericAssertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc))
    else return ()
DocAssertion(assertEqualNoShow, Fail if the two values of type @a@ are not equal.
             The first parameter denotes the expected value. Use these two functions
             of @a@ is neither an instance of 'Show' nor 'Pretty'. Be aware that in this
             case the generated error message might not be very helpful.)
CreateAssertionsCtx(assertEqualNoShow, Eq a, (Eq a, AssertM m), a -> a)

_assertNotEqualNoShow_ :: (Eq a, AssertM m)
                    => String -> Location -> String -> a -> a -> m ()
_assertNotEqualNoShow_ name loc s expected actual =
    if expected == actual
       then genericAssertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc))
       else return ()
DocAssertion(assertNotEqualNoShow, Fail if the two values of type @a@ are equal.
             The first parameter denotes the expected value. Use these two functions
             of @a@ is neither an instance of 'Show' nor 'Pretty'. Be aware that in this
             case the generated error message might not be very helpful.)
CreateAssertionsCtx(assertNotEqualNoShow, Eq a, (Eq a, AssertM m), a -> a)

--
-- Assertions on Lists
--

_assertListsEqualAsSets_ :: (Eq a, Show a, AssertM m)
                   => String -> Location -> String -> [a] -> [a] -> m ()
_assertListsEqualAsSets_ name loc s expected actual =
    let ne = length expected
        na = length actual
        in case () of
            _| ne /= na ->
                 do let x = equalityFailedMessage (show expected) (show actual)
                    genericAssertFailure__ loc (mkColorMsg name s
                                                (noColor
                                                 ("failed at " ++ showLoc loc
                                                  ++ "\n expected length: " ++ show ne
                                                  ++ "\n actual length: " ++ show na) +++
                                                  (if maxLength x < 5000
                                                   then x else emptyColorString)))
             | not (unorderedEq expected actual) ->
                 do let x = equalityFailedMessage (show expected) (show actual)
                    genericAssertFailure__ loc (mkColorMsg "assertSetEqual" s
                                                (noColor ("failed at " ++ showLoc loc) +++ x))
             | otherwise -> return ()
    where unorderedEq l1 l2 =
              null (l1 \\ l2) && null (l2 \\ l1)
DocAssertion(assertListsEqualAsSets, Fail if the two given lists are not equal
                                     when considered as sets. The first list parameter
                                     denotes the expected value.)
CreateAssertionsCtx(assertListsEqualAsSets, (Eq a, Show a), (Eq a, Show a, AssertM m), [a] -> [a])

_assertNotEmpty_ :: AssertM m => String -> Location -> String -> [a] -> m ()
_assertNotEmpty_ name loc s [] =
    genericAssertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc))
_assertNotEmpty_ _ _ _ (_:_) = return ()
DocAssertion(assertNotEmpty, Fail if the given list is empty.)
CreateAssertions(assertNotEmpty, [a])

_assertEmpty_ :: AssertM m => String -> Location -> String -> [a] -> m ()
_assertEmpty_ name loc s (_:_) =
    genericAssertFailure__ loc (mkMsg name s ("failed at " ++ showLoc loc))
_assertEmpty_ _ _ _ [] = return ()
DocAssertion(assertEmpty, Fail if the given list is a non-empty list.)
CreateAssertions(assertEmpty, [a])

_assertElem_ :: (Eq a, Show a, AssertM m) => String -> Location -> String -> a -> [a] -> m ()
_assertElem_ name loc s x l =
    if x `elem` l
    then return ()
    else genericAssertFailure__ loc (mkMsg name s
                                     ("failed at " ++ showLoc loc ++
                                      "\n element: " ++ show x ++
                                      "\n list:   " ++ show l))
DocAssertion(assertElem, Fail if the given element is not in the list.)
CreateAssertionsCtx(assertElem, (Eq a, Show a), (Eq a, Show a, AssertM m), a -> [a])

--
-- Assertions for Exceptions
--

_assertThrowsIO_ :: Exception e
                 => String -> Location -> String -> IO a -> (e -> Bool) -> IO ()
_assertThrowsIO_ name loc s x f =
    _assertThrowsM_ name loc s x f
DocAssertionNoGVariant(assertThrowsIO, Fail if executing the 'IO' action does not
                       throw an exception satisfying the given predicate @(e -> Bool)@.)
CreateAssertionsCtxNoGVariant(assertThrowsIO, Exception e, IO a -> (e -> Bool))

_assertThrowsSomeIO_ :: String -> Location -> String -> IO a -> IO ()
_assertThrowsSomeIO_ name loc s x = _assertThrowsIO_ name loc s x (\ (_e::SomeException) -> True)
DocAssertionNoGVariant(assertThrowsSomeIO, Fail if executing the 'IO' action does not
                       throw an exception.)
CreateAssertionsNoGVariant(assertThrowsSomeIO, IO a)

_assertThrowsM_ :: (MonadBaseControl IO m, MonadIO m, Exception e)
                => String -> Location -> String -> m a -> (e -> Bool) -> m ()
_assertThrowsM_ name loc s x f =
    do res <- ExL.try x
       case res of
         Right _ -> liftIO $
                    genericAssertFailure__ loc (mkMsg name s
                                                ("failed at " ++ showLoc loc ++
                                                 ": no exception was thrown"))
         Left e -> if f e then return ()
                   else liftIO $
                        genericAssertFailure__ loc (mkMsg name s
                                                    ("failed at " ++
                                                     showLoc loc ++
                                                     ": wrong exception was thrown: " ++
                                                     show e))
DocAssertionNoGVariant(assertThrowsM, Fail if executing the 'm' action does not
                       throw an exception satisfying the given predicate @(e -> Bool)@.)
CreateAssertionsGenericNoGVariant(assertThrowsM, (MonadBaseControl IO m, MonadIO m, Exception e) =>,
                                  m a -> (e -> Bool), m ())

_assertThrowsSomeM_ :: (MonadBaseControl IO m, MonadIO m)
                    => String -> Location -> String -> m a -> m ()
_assertThrowsSomeM_ name loc s x = _assertThrowsM_ name loc s x (\ (_e::SomeException) -> True)
DocAssertionNoGVariant(assertThrowsSomeM, Fail if executing the 'm' action does not
                       throw an exception.)
CreateAssertionsGenericNoGVariant(assertThrowsSomeM, (MonadBaseControl IO m, MonadIO m) =>, m a, m ())

_assertThrows_ :: Exception e
               => String -> Location -> String -> a -> (e -> Bool) -> IO ()
_assertThrows_ name loc s x f = _assertThrowsIO_ name loc s (evaluate x) f
DocAssertionNoGVariant(assertThrows, Fail if evaluating the expression of type @a@ does not
                       throw an exception satisfying the given predicate @(e -> Bool)@.)
CreateAssertionsCtxNoGVariant(assertThrows, Exception e, a -> (e -> Bool))

_assertThrowsSome_ :: String -> Location -> String -> a -> IO ()
_assertThrowsSome_ name loc s x =
    _assertThrows_ name loc s x (\ (_e::SomeException) -> True)
DocAssertionNoGVariant(assertThrowsSome, Fail if evaluating the expression of type @a@ does not
                       throw an exception.)
CreateAssertionsNoGVariant(assertThrowsSome, a)

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
DocAssertion(assertLeft, Fail if the given @Either a b@ value is a 'Right'.
             Use this function if @b@ is an instance of 'Show')
CreateAssertionsCtxRet(assertLeft, Show b, (Show b, AssertM m), Either a b, a)

_assertLeftNoShow_ :: AssertM m => String -> Location -> String -> Either a b -> m a
_assertLeftNoShow_ _ _ _ (Left x) = return x
_assertLeftNoShow_ name loc s (Right _) =
    genericAssertFailure__ loc (mkMsg name s
                                ("failed at " ++ showLoc loc ++
                                 ": expected a Left value, given a Right value"))
DocAssertion(assertLeftNoShow, Fail if the given @Either a b@ value is a 'Right'.)
CreateAssertionsRet(assertLeftNoShow, Either a b, a)

_assertRight_ :: forall a b m . (Show a, AssertM m)
              => String -> Location -> String -> Either a b -> m b
_assertRight_ _ _ _ (Right x) = return x
_assertRight_ name loc s (Left x) =
    genericAssertFailure__ loc (mkMsg name s
                                ("failed at " ++ showLoc loc ++
                                 ": expected a Right value, given " ++
                                 show (Left x :: Either a a)))
DocAssertion(assertRight, Fail if the given @Either a b@ value is a 'Left'.
             Use this function if @a@ is an instance of 'Show')
CreateAssertionsCtxRet(assertRight, Show a, (Show a, AssertM m), Either a b, b)

_assertRightNoShow_ :: AssertM m => String -> Location -> String -> Either a b -> m b
_assertRightNoShow_ _ _ _ (Right x) = return x
_assertRightNoShow_ name loc s (Left _) =
    genericAssertFailure__ loc (mkMsg name s
                                ("failed at " ++ showLoc loc ++
                                 ": expected a Right value, given a Left value"))
DocAssertion(assertRightNoShow, Fail if the given @Either a b@ value is a 'Left'.)
CreateAssertionsRet(assertRightNoShow, Either a b, b)

--
-- Assertions on Maybe
--

_assertJust_ :: AssertM m => String -> Location -> String -> Maybe a -> m a
_assertJust_ _ _ _ (Just x) = return x
_assertJust_ name loc s Nothing =
    genericAssertFailure__ loc (mkMsg name s
                                ("failed at " ++ showLoc loc ++
                                 ": expected a Just value, given Nothing"))
DocAssertion(assertJust, Fail is the given @Maybe a@ value is a 'Nothing'.)
CreateAssertionsRet(assertJust, Maybe a, a)

_assertNothing_ :: (Show a, AssertM m)
                => String -> Location -> String -> Maybe a -> m ()
_assertNothing_ _ _ _ Nothing = return ()
_assertNothing_ name loc s jx =
    genericAssertFailure__ loc (mkMsg name s
                                ("failed at " ++ showLoc loc ++
                                 ": expected Nothing, given " ++ show jx))
DocAssertion(assertNothing, Fail is the given @Maybe a@ value is a 'Just'.
             Use this function if @a@ is an instance of 'Show'.)
CreateAssertionsCtx(assertNothing, Show a, (Show a, AssertM m), Maybe a)

_assertNothingNoShow_ :: AssertM m => String -> Location -> String -> Maybe a -> m ()
_assertNothingNoShow_ _ _ _ Nothing = return ()
_assertNothingNoShow_ name loc s _ =
    genericAssertFailure__ loc (mkMsg name s
                                ("failed at " ++ showLoc loc ++
                                 ": expected Nothing, given a Just value"))
DocAssertion(assertNothingNoShow, Fail is the given @Maybe a@ value is a 'Just'.)
CreateAssertions(assertNothingNoShow, Maybe a)

--
-- Sub assertions
--

-- | Sub assertions are a poor man's way of abstracting over assertions while still propagating location
-- information. Say you want to abstract over the assertion that an 'Int' is positive. You would write
--
-- > assertIsPositive :: Int -> Assertion
-- > assertIsPositive n = assertBool (n > 0)
--
-- You can now use @assertIsPositive i@ for some integer @i@ from your unit tests, but if you call it directly
-- you will lose location information: if @assertIsPositive i@ fails you will only get the location where
-- @assertIsPositive@ is defined but not from where it has been called.
--
-- To recover the location information you simply use @subAssert (assertIsPositive i)@.
-- In this case, if @i@ is not positive, you will get the location of the caller.
--
-- /Note:/ Don't use subAssert_ directly but use the preprocessor macro @subAssert@.
subAssert_ :: MonadBaseControl IO m => Location -> m a -> m a
subAssert_ loc ass = subAssertHTF loc Nothing ass

-- | Generic variant of 'subAssert_'.
gsubAssert_ :: AssertM m => Location -> m a -> m a
gsubAssert_ loc ass = genericSubAssert loc Nothing ass

-- | Same as 'subAssert_' but with an additional error message.
subAssertVerbose_ :: MonadBaseControl IO m => Location -> String -> m a -> m a
subAssertVerbose_ loc msg ass = subAssertHTF loc (Just msg) ass

-- | Generic variant of 'subAssertVerbose_'.
gsubAssertVerbose_ :: AssertM m => Location -> String -> m a -> m a
gsubAssertVerbose_ loc msg ass = genericSubAssert loc (Just msg) ass
