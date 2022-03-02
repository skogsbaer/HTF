{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

{- |

This module defines the API for HTF plugins.

-}

module Test.Framework.TestInterface (

    Assertion, TestResult(..), FullTestResult(..), HTFFailureException(..)
  , HtfStackEntry(..), HtfStack, emptyHtfStack, mkHtfStack, formatHtfStack
  , failureLocationFromStack, failureLocation
  , restCallStack, htfStackToList
  , failHTF, subAssertHTF, addCallerToSubAssertStack
  , mkFullTestResult

) where

import Test.Framework.Location
import Test.Framework.Colors

import Control.Monad.Trans.Control
import Data.Typeable
import GHC.Stack
import qualified Data.List as L
import qualified Control.Exception as Exc
import qualified Control.Exception.Lifted as ExcLifted

{- | An assertion is just an 'IO' action. Internally, the body of any test
in HTF is of type 'Assertion'. If a test specification of a certain plugin
has a type different from 'Assertion', the plugin's preprocessor pass must
inject wrapper code to convert the test specification into an assertion.

Assertions may use 'failHTF' to signal a 'TestResult' different from
'Pass'. If the assertion finishes successfully, the tests passes
implicitly.

Please note: the assertion must not swallow any exceptions! Otherwise,
timeouts and other things might not work as expected.
-}
type Assertion = IO ()

-- | The summary result of a test.
data TestResult = Pass | Pending | Fail | Error
                deriving (Show, Read, Eq)

data HtfStackEntry
    = HtfStackEntry
    { hse_location :: Location
    , hse_calledFunction :: String
    , hse_message :: Maybe String
    } deriving (Eq, Ord, Show, Read)

-- The first entry in the list is the location of the assertion failure
data HtfStack
    = HtfStack
      { hs_assertStack :: [HtfStackEntry]
      , hs_subAssertStack :: [HtfStackEntry]
      }
    deriving (Eq, Ord, Show, Read)

mkHtfStack :: CallStack -> HtfStack
mkHtfStack cs = HtfStack (map mkHtfStackEntry (removeHtfPrefix (getCallStack cs))) []

removeHtfPrefix :: [(String, SrcLoc)] -> [(String, SrcLoc)]
removeHtfPrefix [] = []
removeHtfPrefix all@((_, srcLoc) : rest) =
    if "Test.Framework" `L.isPrefixOf` srcLocModule srcLoc
    then removeHtfPrefix rest
    else all

mkHtfStackEntry :: (String, SrcLoc) -> HtfStackEntry
mkHtfStackEntry x = mkHtfStackEntry' x Nothing

mkHtfStackEntry' :: (String, SrcLoc) -> Maybe String -> HtfStackEntry
mkHtfStackEntry' (funName, srcLoc) mMsg =
    HtfStackEntry
    { hse_location = makeLoc (srcLocFile srcLoc) (srcLocStartLine srcLoc)
    , hse_calledFunction = funName
    , hse_message = mMsg
    }

htfStackToList :: HtfStack -> [HtfStackEntry]
htfStackToList s = hs_assertStack s ++ reverse (hs_subAssertStack s)

emptyHtfStack :: HtfStack
emptyHtfStack = HtfStack [] []

failureLocation :: HasCallStack => Maybe Location
failureLocation = failureLocationFromStack (mkHtfStack callStack)

failureLocationFromStack :: HtfStack -> Maybe Location
failureLocationFromStack stack =
    case htfStackToList stack of
      [] -> Nothing
      e:_ -> Just (hse_location e)

restCallStack :: HtfStack -> [HtfStackEntry]
restCallStack stack =
    case htfStackToList stack of
      [] -> []
      _:rest -> rest

-- | Formats a stack trace.
formatHtfStack :: HtfStack -> String
formatHtfStack stack =
    unlines $ map formatStackElem $ zip [0..] $ htfStackToList stack
    where
      formatStackElem (pos, HtfStackEntry loc _ mMsg) =
          let pref = if pos > 0 then "  called from " else "  at "
          in pref ++ showLoc loc ++ showMsg mMsg
      showMsg Nothing = ""
      showMsg (Just m) = " (" ++ m ++ ")"

-- | The full result of a test, as used by HTF plugins.
data FullTestResult
    = FullTestResult
      { ftr_stack :: HtfStack                  -- ^ The stack to the location of a possible failure
      , ftr_message :: Maybe ColorString       -- ^ An error message
      , ftr_result :: Maybe TestResult         -- ^ The outcome of the test, 'Nothing' means timeout
      } deriving (Eq, Show, Read)

-- | Auxiliary function for contructing a 'FullTestResult'.
mkFullTestResult :: TestResult -> Maybe String -> FullTestResult
mkFullTestResult r msg =
    FullTestResult
    { ftr_stack = emptyHtfStack
    , ftr_message = fmap noColor msg
    , ftr_result = Just r
    }

-- Internal exception type for propagating exceptions.
data HTFFailureException
    = HTFFailure FullTestResult
      deriving (Show, Typeable)

instance Exc.Exception HTFFailureException

{- |
Terminate a HTF test, usually to signal a failure. The result of the test
is given in the 'FullTestResult' argument.
-}
failHTF :: MonadBaseControl IO m => FullTestResult -> m a
-- Important: force the string argument, otherwise an error embedded
-- lazily inside the string might escape.
failHTF r = length (show r) `seq` ExcLifted.throwIO (HTFFailure r)

addCallerToSubAssertStack :: CallStack -> HtfStack -> Maybe String -> HtfStack
addCallerToSubAssertStack ghcStack stack@(HtfStack s1 s2) mMsg =
    case removeHtfPrefix (getCallStack ghcStack) of
      [] -> stack
      (entry : _) -> HtfStack s1 ((mkHtfStackEntry' entry mMsg) : s2)

{- |
Opens a new assertion stack frame to allow for sensible location information.
This function should be used if the function being called does not carry
a 'HasCallStack' annotation.
-}
subAssertHTF :: (HasCallStack, MonadBaseControl IO m) => Maybe String -> m a -> m a
subAssertHTF mMsg action =
    let stack = callStack
    in action `ExcLifted.catch`
                  (\(HTFFailure res) ->
                       let newRes =
                               res { ftr_stack =
                                         addCallerToSubAssertStack stack (ftr_stack res) mMsg }
                       in failHTF newRes)
