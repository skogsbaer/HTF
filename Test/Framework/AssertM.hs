{-# LANGUAGE CPP #-}
--
-- Copyright (c) 2005-2022   Stefan Wehr - http://www.stefanwehr.de
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

This module defines the 'AssertM' monad, which allows you either to run assertions
as ordinary unit tests or to evaluate them as pure functions.

-}
module Test.Framework.AssertM (

    AssertM(..), AssertBool(..), boolValue, eitherValue

) where

import Control.Monad       (liftM, ap)
import GHC.Stack
import qualified Data.Text as T

import Test.Framework.TestInterface
import Test.Framework.Colors

-- | A typeclass for generic assertions.
class Monad m => AssertM m where
    genericAssertFailure :: HasCallStack => ColorString -> m a
    genericSubAssert :: HasCallStack => Maybe String -> m a -> m a

instance AssertM IO where
    genericAssertFailure s =
        failHTF (FullTestResult (mkHtfStack callStack) (Just s) (Just Fail))
    genericSubAssert mMsg action = subAssertHTF mMsg action

-- | Type for evaluating a generic assertion as a pure function.
data AssertBool a
    -- | Assertion passes successfully and yields the given value.
    = AssertOk a
    -- | Assertion fails with the given stack trace. In the stack trace, the outermost stackframe comes first.
    | AssertFailed HtfStack String
      deriving (Eq, Ord, Show, Read)

instance Functor AssertBool where
    fmap = liftM

instance Applicative AssertBool where
    pure  = AssertOk
    (<*>) = ap

instance Monad AssertBool where
    return = AssertOk
    AssertFailed stack msg >>= _ = AssertFailed stack msg
    AssertOk x >>= k = k x
#if !(MIN_VERSION_base(4,13,0))
    fail msg = AssertFailed emptyHtfStack msg
#endif

instance AssertM AssertBool where
    genericAssertFailure s =
        AssertFailed (mkHtfStack callStack) (T.unpack $ renderColorString s False)
    genericSubAssert subMsg action =
        case action of
          AssertOk x -> AssertOk x
          AssertFailed stack msg ->
              let ghcStack = callStack
              in AssertFailed (addCallerToSubAssertStack ghcStack stack subMsg) msg

-- | Evaluates a generic assertion to a 'Bool' value.
boolValue :: AssertBool a -> Bool
boolValue x =
    case x of
      AssertOk _ -> True
      AssertFailed _ _ -> False

-- | Evaluates a generic assertion to an 'Either' value. The result
--   is @Right x@ if the assertion passes and yields value @x@, otherwise
--   the result is @Left err@, where @err@ is an error message.
eitherValue :: AssertBool a -> Either String a
eitherValue x =
    case x of
      AssertOk z -> Right z
      AssertFailed stack msg -> Left (msg ++ "\n" ++ formatHtfStack stack)
