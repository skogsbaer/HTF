{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

--
-- Copyright (c) 2005-2022 Stefan Wehr - http://www.stefanwehr.de
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

This module integrates the 'Test.QuickCheck' library into HTF. It re-exports
all functionality of 'Test.QuickCheck' and defines some additional functions.

-}

module Test.Framework.QuickCheckWrapper (

  module Test.QuickCheck,

  -- * Arguments for evaluating properties
  defaultArgs, getCurrentArgs, setDefaultArgs,
  withQCArgs, WithQCArgs, setReplayFromString,
  QCAssertion,

  -- * Pending properties
  qcPending,

  -- * Auxiliary functions
#if !MIN_VERSION_QuickCheck(2,7,0)
  ioProperty,
#endif
  assertionAsProperty,

  -- * Internal functions
  qcAssertion

) where

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding ( catch )
#endif
import Control.Exception ( SomeException, Exception, Handler(..),
                           throw, catch, catches, evaluate )
import Data.Typeable (Typeable)
import Data.Char
import qualified Data.List as List
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

import Test.QuickCheck
#if !MIN_VERSION_QuickCheck(2,7,0)
import Test.QuickCheck.Property (morallyDubiousIOProperty)
#endif
import Test.Framework.TestInterface
import Test.Framework.Utils

_DEBUG_ :: Bool
_DEBUG_ = False

debug :: String -> IO ()
debug s = if _DEBUG_ then putStrLn ("[DEBUG] " ++ s) else return ()

data QCState = QCState { qc_args :: !Args }

qcState :: IORef QCState
qcState = unsafePerformIO (newIORef (QCState defaultArgs))
{-# NOINLINE qcState #-}

-- | The 'Args' used if not explicitly changed.
defaultArgs :: Args
defaultArgs = stdArgs { chatty = False }

-- | Change the default 'Args' used to evaluate quick check properties.
setDefaultArgs :: Args -> IO ()
setDefaultArgs args =
    do force <- atomicModifyIORef qcState $ \state ->
                  let newState = state { qc_args = args }
                  in (newState, newState)
       force `seq` return ()

-- | Retrieve the 'Args' currently used per default when evaluating quick check properties.
getCurrentArgs :: IO Args
getCurrentArgs =
    do state <- readIORef qcState
       return (qc_args state)

data QCPendingException = QCPendingException String
                        deriving (Show,Read,Eq,Typeable)

instance Exception QCPendingException

quickCheckTestError :: Maybe String -> Assertion
quickCheckTestError m = failHTF $ mkFullTestResult Error m

quickCheckTestFail :: Maybe String -> Assertion
quickCheckTestFail m = failHTF $ mkFullTestResult Fail m

quickCheckTestPending :: String -> Assertion
quickCheckTestPending m = failHTF $ mkFullTestResult Pending (Just m)

quickCheckTestPass :: String -> Assertion
quickCheckTestPass m = failHTF $ mkFullTestResult Pass (Just m)

-- | Turns a 'Test.QuickCheck' property into an 'Assertion'. This function
-- is used internally in the code generated by @htfpp@, do not use it directly.
qcAssertion :: (QCAssertion t) => t -> Assertion
qcAssertion qc =
    do origArgs <- getCurrentArgs
       eitherArgs <-
           (let a = (argsModifier qc) origArgs
            in do _ <- evaluate (length (show a))
                  return (Right a))
           `catch`
           (\e -> return $ Left (show (e :: SomeException)))
       case eitherArgs of
         Left err -> quickCheckTestError
                        (Just ("Cannot evaluate custom arguments: "
                               ++ err))
         Right args ->
             do res <- do anyTestable <- evaluate (testable qc)
                          x <- case anyTestable of
                                 AnyTestable t' -> quickCheckWithResult args t'
                          return (Right x)
                      `catches`
                       [Handler $ \(QCPendingException msg) -> return $ Left msg]
                debug ("QuickCheck result: " ++ show res)
                case res of
                  Left err ->
                      quickCheckTestPending err
                  Right (Success { output=msg }) ->
                      quickCheckTestPass (adjustOutput msg)
                  Right (Failure { usedSize=size, usedSeed=gen, output=msg, reason=reason }) ->
                      case () of
                        _| pendingPrefix `List.isPrefixOf` reason ->
                             let pendingMsg = getPayload pendingPrefix pendingSuffix reason
                             in quickCheckTestPending pendingMsg
                         | failurePrefix `List.isPrefixOf` reason
                         , Just result <- readM (getPayload failurePrefix failureSuffix reason)
                            -> failHTF result
                         | otherwise ->
                             let replay = "Replay argument: " ++ (show (show (Just (gen, size))))
                                 out = adjustOutput msg
                             in quickCheckTestFail (Just (out ++ "\n" ++ replay))
                  Right (GaveUp { output=msg }) ->
                      quickCheckTestFail (Just (adjustOutput msg))
                  Right (NoExpectedFailure { output=msg }) ->
                      quickCheckTestFail (Just (adjustOutput msg))
#if MIN_VERSION_QuickCheck(2,8,0) && !MIN_VERSION_QuickCheck(2,12,0)
                  Right (InsufficientCoverage { output=msg }) ->
                      quickCheckTestFail (Just (adjustOutput msg))
#endif
                return ()
    where
      pendingPrefix = "Exception: 'QCPendingException \""
      pendingSuffix = "\"'"
      failurePrefix = "Exception: 'HTFFailure "
      failureSuffix = "'"
      getPayload pref suf reason =
          let s = drop (length pref) reason
          in take (length s - length suf) s
      adjustOutput s = trimTrailing $
          case s of
            '+':'+':'+':' ':'O':'K':',':' ':'p':rest -> 'P':rest
            '*':'*':'*':' ':'F':'a':'i':'l':'e':'d':'!':' ':rest -> rest
            '*':'*':'*':' ':rest -> rest
            _ -> s
      trimTrailing = reverse . dropWhile isSpace . reverse

-- | Abstract type for representing quick check properties with custom 'Args'.
--   Used only internally.
data WithQCArgs a = WithQCArgs (Args -> Args) a

-- | Existential holding a 'Testable' value.
--   Used only internally.
data AnyTestable = forall a . Testable a => AnyTestable a

-- | Type class providing access to the custom 'Args' of a quick check property.
--   Used only internally.
class QCAssertion a where
    argsModifier :: a -> (Args -> Args)
    testable :: a -> AnyTestable

instance {-# OVERLAPPABLE #-} Testable a => QCAssertion a where
    argsModifier _ = id
    testable = AnyTestable

instance {-# OVERLAPPING  #-} Testable a => QCAssertion (WithQCArgs a) where
    argsModifier (WithQCArgs f _) = f
    testable (WithQCArgs _ x) = AnyTestable x

-- | Run a 'Test.QuickCheck' property with modified quick check arguments 'Args'.
withQCArgs :: (Testable a) => (Args -> Args) -- ^ Modification function for the default 'Args'
           -> a                              -- ^ Property
           -> WithQCArgs a
withQCArgs = WithQCArgs

-- | Use @qcPending msg prop@ to mark the given quick check property as pending
-- without removing it from the test suite and without deleting or commenting out the property code.
qcPending :: Testable t => String -> t -> t
qcPending msg _ = throw (QCPendingException msg)

#if !MIN_VERSION_QuickCheck(2,7,0)
ioProperty :: Testable prop => IO prop -> Property
ioProperty = morallyDubiousIOProperty
#endif

assertionAsProperty :: IO () -> Property
assertionAsProperty action =
    ioProperty $ action >> return True

-- | Sets the 'replay' parameter of the 'Args' datatype by parsing the given string.
setReplayFromString :: Args -> String -> Args
setReplayFromString args str =
#if !MIN_VERSION_QuickCheck(2,7,0)
    case readM str of
      Just x -> args { replay = x }
      Nothing -> error ("Could not parse replay parameter from string " ++ show str)
#else
    -- Starting with QC 2.7 the type of the replay field changed from
    -- 'Maybe (StdGen, Int)' to 'Maybe (QCGen, Int)'
    case readM str of
      Just x -> args { replay = x }
      Nothing ->
          error ("Could not parse replay parameter from string " ++ show str)
#endif
