{-# OPTIONS_GHC -cpp -pgmPcpphs -optP --layout -optP --hashes -optP --cpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
--
-- Copyright (c) 2005, 2009   Stefan Wehr - http://www.stefanwehr.de
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
You should not use the functions provided by this module directly.
Instead, for each function @assertXXX_@ defined in this module,
there exist a preprocessor macro @assertXXX@, which provides
the "Location" parameter automatically.
|-}

module Test.Framework.HUnitWrapper (

  -- * General failure
  assertFailure,

  -- * Assertions on Bool values
  assertBool_, assertBoolVerbose_,

  -- * Equality assertions
  assertEqual_, assertEqualVerbose_,
  assertEqualPretty_, assertEqualPrettyVerbose_,
  assertEqualNoShow_, assertEqualNoShowVerbose_,

  -- * Assertions on lists
  assertListsEqualAsSets_, assertListsEqualAsSetsVerbose_,
  assertNotEmpty_, assertNotEmptyVerbose_,
  assertEmpty_, assertEmptyVerbose_,

  -- * Assertions for exceptions
  assertThrows_, assertThrowsVerbose_,
  assertThrowsSome_, assertThrowsSomeVerbose_,

  -- * Assertions on Either values
  assertLeft_, assertLeftVerbose_,
  assertLeftNoShow_, assertLeftNoShowVerbose_,
  assertRight_, assertRightVerbose_,
  assertRightNoShow_, assertRightNoShowVerbose_,

  -- * Assertions on Just values
  assertJust_, assertJustVerbose_,
  assertNothing_, assertNothingVerbose_,
  assertNothingNoShow_, assertNothingNoShowVerbose_

) where

import System.IO ( stderr )
import Data.List ( (\\) )
import Control.Exception
import Control.Monad
import qualified Test.HUnit as HU hiding ( assertFailure )

import Test.Framework.TestManager
import Test.Framework.Location
import Test.Framework.Utils
import Test.Framework.Pretty

-- WARNING: do not forget to add a preprocessor macro for new assertions!!

assertFailure :: String -> IO a
assertFailure s = unitTestFail s

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

--
-- Boolean Assertions
--
_assertBool_ :: String -> Location -> String -> Bool -> HU.Assertion
_assertBool_ name loc s False =
    assertFailure (mkMsg name s ("failed at " ++ showLoc loc))
_assertBool_ _ _ _   True = return ()
CreateAssertions(assertBool, Bool)

--
-- Equality Assertions
--
_assertEqual_ :: (Eq a, Show a)
                 => String -> Location -> String -> a -> a -> HU.Assertion
_assertEqual_ name loc s expected actual =
    if expected /= actual
       then assertFailure (mkMsg name s msg)
       else return ()
    where msg = "failed at " ++ showLoc loc ++
                "\n expected: " ++ show expected ++
                "\n but got:  " ++ show actual
CreateAssertionsCtx(assertEqual, (Eq a, Show a), a -> a)

_assertEqualPretty_ :: (Eq a, Pretty a)
                       => String -> Location -> String -> a -> a -> HU.Assertion
_assertEqualPretty_ name loc s expected actual =
    if expected /= actual
       then assertFailure (mkMsg name s msg)
       else return ()
    where msg = "assertEqual failed at " ++ showLoc loc ++
                "\n expected:\n" ++ showPretty expected ++
                "\n but got:\n" ++ showPretty actual
CreateAssertionsCtx(assertEqualPretty, (Eq a, Pretty a), a -> a)

_assertEqualNoShow_ :: Eq a
                    => String -> Location -> String -> a -> a -> HU.Assertion
_assertEqualNoShow_ name loc s expected actual =
    if expected /= actual
       then assertFailure (mkMsg name s ("failed at " ++ showLoc loc))
       else return ()
CreateAssertionsCtx(assertEqualNoShow, Eq a, a -> a)

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
                 assertFailure (mkMsg name s
                                ("failed at " ++ showLoc loc
                                 ++ "\n expected length: " ++ show ne
                                 ++ "\n actual length: " ++ show na))
             | not (unorderedEq expected actual) ->
                 assertFailure (mkMsg "assertSetEqual" s
                                ("failed at " ++ showLoc loc
                                 ++ "\n expected: " ++ show expected
                                 ++ "\n actual: " ++ show actual))
             | otherwise -> return ()
    where unorderedEq l1 l2 =
              null (l1 \\ l2) && null (l2 \\ l1)
CreateAssertionsCtx(assertListsEqualAsSets, (Eq a, Show a), [a] -> [a])

_assertNotEmpty_ :: String -> Location -> String -> [a] -> HU.Assertion
_assertNotEmpty_ name loc s [] =
    assertFailure (mkMsg name s ("failed at " ++ showLoc loc))
_assertNotEmpty_ _ _ _ (_:_) = return ()
CreateAssertions(assertNotEmpty, [a])

_assertEmpty_ :: String -> Location -> String -> [a] -> HU.Assertion
_assertEmpty_ name loc s (_:_) =
    assertFailure (mkMsg name s ("failed at " ++ showLoc loc))
_assertEmpty_ _ _ _ [] = return ()
CreateAssertions(assertEmpty, [a])

--
-- Assertions for Exceptions
--

_assertThrows_ :: Exception e
               => String -> Location -> String -> a -> (e -> Bool) -> HU.Assertion
_assertThrows_ name loc s x f =
    do res <- try (evaluate x)
       case res of
         Right _ -> assertFailure (mkMsg name s
                                   ("failed at " ++ showLoc loc ++
                                    ": no exception was thrown"))
         Left e -> if f e then return ()
                   else assertFailure (mkMsg name s
                                       ("failed at " ++
                                        showLoc loc ++
                                        ": wrong exception was thrown: " ++
                                        show e))
CreateAssertionsCtx(assertThrows, Exception e, a -> (e -> Bool))

_assertThrowsSome_ :: String -> Location -> String -> a -> HU.Assertion
_assertThrowsSome_ name loc s x =
    _assertThrows_ name loc s x (\ (e::SomeException) -> True)
CreateAssertions(assertThrowsSome, a)

--
-- Assertions on Either
--

_assertLeft_ :: forall a b . Show b
             => String -> Location -> String -> Either a b -> IO a
_assertLeft_ _ _ _ (Left x) = return x
_assertLeft_ name loc s (Right x) =
    assertFailure (mkMsg name s
                   ("failed at " ++ showLoc loc ++
                    ": expected a Left value, given " ++
                    show (Right x :: Either b b)))
CreateAssertionsCtxRet(assertLeft, Show b, Either a b, IO a)

_assertLeftNoShow_ :: String -> Location -> String -> Either a b -> IO a
_assertLeftNoShow_ _ _ _ (Left x) = return x
_assertLeftNoShow_ name loc s (Right _) =
    assertFailure (mkMsg name s ("failed at " ++ showLoc loc ++
                                 ": expected a Left value, given a Right value"))
CreateAssertionsRet(assertLeftNoShow, Either a b, IO a)

_assertRight_ :: forall a b . Show a
              => String -> Location -> String -> Either a b -> IO b
_assertRight_ _ _ _ (Right x) = return x
_assertRight_ name loc s (Left x) =
    assertFailure (mkMsg name s ("failed at " ++ showLoc loc ++
                                 ": expected a Right value, given " ++
                                 show (Left x :: Either a a)))
CreateAssertionsCtxRet(assertRight, Show a, Either a b, IO b)

_assertRightNoShow_ :: String -> Location -> String -> Either a b -> IO b
_assertRightNoShow_ _ _ _ (Right x) = return x
_assertRightNoShow_ name loc s (Left _) =
    assertFailure (mkMsg name s ("failed at " ++ showLoc loc ++
                                 ": expected a Right value, given a Left value"))
CreateAssertionsRet(assertRightNoShow, Either a b, IO b)

--
-- Assertions on Maybe
--

_assertJust_ :: String -> Location -> String -> Maybe a -> IO a
_assertJust_ _ _ _ (Just x) = return x
_assertJust_ name loc s Nothing =
    assertFailure (mkMsg name s ("failed at " ++ showLoc loc ++
                                 ": expected a Just value, given Nothing"))
CreateAssertionsRet(assertJust, Maybe a, IO a)

_assertNothing_ :: Show a
                => String -> Location -> String -> Maybe a -> HU.Assertion
_assertNothing_ _ _ _ Nothing = return ()
_assertNothing_ name loc s jx =
    assertFailure (mkMsg name s ("failed at " ++ showLoc loc ++
                                 ": expected Nothing, given " ++ show jx))
CreateAssertionsCtx(assertNothing, Show a, Maybe a)

_assertNothingNoShow_ :: String -> Location -> String -> Maybe a -> HU.Assertion
_assertNothingNoShow_ _ _ _ Nothing = return ()
_assertNothingNoShow_ name loc s _ =
    assertFailure (mkMsg name s ("failed at " ++ showLoc loc ++
                                 ": expected Nothing, given a Just value"))
CreateAssertions(assertNothingNoShow, Maybe a)
