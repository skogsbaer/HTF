{-|

This module defines the 'AssertM' monad, which allows you either to run assertions
as ordinary unit tests or to evaluate them as pure functions.

-}
module Test.Framework.AssertM (

    AssertM(..), AssertStackElem(..), AssertBool(..), boolValue, eitherValue, formatStack

) where

import Data.Maybe
import qualified Data.Text as T

import Test.Framework.TestInterface
import Test.Framework.Location
import Test.Framework.Colors

-- | A typeclass for generic assertions.
class Monad m => AssertM m where
    genericAssertFailure__ :: Location -> ColorString -> m a
    genericSubAssert :: Location -> Maybe String -> m a -> m a

instance AssertM IO where
    genericAssertFailure__ loc s = failHTF (FullTestResult (Just loc) [] (Just s) (Just Fail))
    genericSubAssert loc mMsg action = subAssertHTF loc mMsg action

-- | Stack trace element for generic assertions.
data AssertStackElem
    = AssertStackElem
      { ase_message :: Maybe String
      , ase_location :: Maybe Location
      }
      deriving (Eq, Ord, Show, Read)

-- | Type for evaluating a generic assertion as a pure function.
data AssertBool a
    -- | Assertion passes successfully and yields the given value.
    = AssertOk a
    -- | Assertion fails with the given stack trace. In the stack trace, the outermost stackframe comes first.
    | AssertFailed [AssertStackElem]
      deriving (Eq, Ord, Show, Read)

instance Monad AssertBool where
    return = AssertOk
    AssertFailed stack >>= _ = AssertFailed stack
    AssertOk x >>= k = k x
    fail msg = AssertFailed [AssertStackElem (Just msg) Nothing]

instance AssertM AssertBool where
    genericAssertFailure__ loc s =
        AssertFailed [AssertStackElem (Just (T.unpack $ renderColorString s False)) (Just loc)]

    genericSubAssert loc mMsg action =
        case action of
          AssertOk x -> AssertOk x
          AssertFailed stack ->
              AssertFailed (AssertStackElem mMsg (Just loc) : stack)

-- | Evaluates a generic assertion to a 'Bool' value.
boolValue :: AssertBool a -> Bool
boolValue x =
    case x of
      AssertOk _ -> True
      AssertFailed _ -> False

-- | Evaluates a generic assertion to an 'Either' value. The result
--   is @Right x@ if the assertion passes and yields value @x@, otherwise
--   the result is @Left err@, where @err@ is an error message.
eitherValue :: AssertBool a -> Either String a
eitherValue x =
    case x of
      AssertOk z -> Right z
      AssertFailed stack -> Left (formatStack stack)

-- | Formats a stack trace.
formatStack :: [AssertStackElem] -> String
formatStack stack =
    unlines $ map formatStackElem $ zip [0..] $ reverse stack
    where
      formatStackElem (pos, AssertStackElem mMsg mLoc) =
          let floc = fromMaybe "<unknown location>" $ fmap showLoc mLoc
              fmsg = fromMaybe "" $ fmap (\s -> ": " ++ s) mMsg
              pref = if pos > 0 then "  called from " else ""
          in pref ++ floc ++ fmsg
