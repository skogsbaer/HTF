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

    Assertion, TestResult(..), FullTestResult(..), HTFFailureException(..), failHTF, subAssertHTF
  , mkFullTestResult

) where

import Test.Framework.Location
import Test.Framework.Colors

import Control.Monad.Trans.Control
import Data.Typeable
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

-- | The full result of a test, as used by HTF plugins.
data FullTestResult
    = FullTestResult
      { ftr_location :: Maybe Location                     -- ^ The location of a possible failure
      , ftr_callingLocations :: [(Maybe String, Location)] -- ^ The "stack" to the location of a possible failure
      , ftr_message :: Maybe ColorString                   -- ^ An error message
      , ftr_result :: Maybe TestResult                     -- ^ The outcome of the test, 'Nothing' means timeout
      } deriving (Eq, Show, Read)

-- | Auxiliary function for contructing a 'FullTestResult'.
mkFullTestResult :: TestResult -> Maybe String -> FullTestResult
mkFullTestResult r msg =
    FullTestResult
    { ftr_location = Nothing
    , ftr_callingLocations = []
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

{- |
Opens a new assertion stack frame to allow for sensible location information.
-}
subAssertHTF :: MonadBaseControl IO m => Location -> Maybe String -> m a -> m a
subAssertHTF loc mMsg action =
    action `ExcLifted.catch`
               (\(HTFFailure res) ->
                    let newRes = res { ftr_callingLocations = (mMsg, loc) : ftr_callingLocations res }
                    in failHTF newRes)
