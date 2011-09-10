{-# LANGUAGE FlexibleInstances,OverlappingInstances,ExistentialQuantification,
             DeriveDataTypeable,ScopedTypeVariables #-}

--
-- Copyright (c) 2005,2009   Stefan Wehr - http://www.stefanwehr.de
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

module Test.Framework.QuickCheckWrapper (

  testableAsAssertion,

  module Test.QuickCheck,

  TestableWithQCArgs, WithQCArgs, withQCArgs, asTestableWithQCArgs,
  qcPending

) where

import qualified Data.Map as Map
import Control.Concurrent.MVar
import Prelude hiding ( catch )
import Control.Exception ( SomeException, Exception, Handler(..),
                           throw, catch, catches, evaluate )
import System.IO
import System.IO.Unsafe
import System.Random
import Data.List( group, sort, intersperse, isPrefixOf )
import Data.Char
import Data.Typeable (Typeable)

import Test.QuickCheck
import Test.QuickCheck.Property hiding (reason)

import Test.Framework.TestManager
import Test.Framework.TestManagerInternal

data QCState = QCState { qc_args :: Args }

qcState :: MVar QCState
qcState = unsafePerformIO (newMVar (QCState defaultArgs))
{-# NOINLINE qcState #-}

defaultArgs :: Args
defaultArgs = stdArgs { chatty = False }

setDefaultArgs :: Args -> IO ()
setDefaultArgs args =
    do withMVar qcState $ \state -> return (state { qc_args = args })
       return ()

getCurrentArgs :: IO Args
getCurrentArgs =
    withMVar qcState $ \state -> return (qc_args state)

data QCPendingException = QCPendingException String
                        deriving (Show,Read,Eq,Typeable)

instance Exception QCPendingException

testableAsAssertion :: (Testable t, WithQCArgs t) => t -> Assertion
testableAsAssertion t =
    withMVar qcState $ \state ->
        do eitherArgs <-
               (let a = (argsModifier t) (qc_args state)
                in do evaluate (length (show a))
                      return (Right a))
               `catch`
               (\e -> return $ Left (show (e :: SomeException)))
           case eitherArgs of
             Left err -> quickCheckTestError
                            (Just ("Cannot evaluate custom arguments: "
                                   ++ err))
             Right args ->
                 do res <- do x <- t `seq` quickCheckWithResult args t
                              return (Right x)
                          `catches`
                           [Handler $ \(QCPendingException msg) -> return $ Left (True, msg)
                           ,Handler $ \(e::SomeException) -> return $ Left (False, show (e::SomeException))]
                    case res of
                      Left (isPending, err) ->
                          if isPending
                             then quickCheckTestPending err
                             else quickCheckTestError (Just err)
                      Right (Success { output=msg }) ->
                          quickCheckTestPass (adjustOutput msg)
                      Right (Failure { usedSize=size, usedSeed=gen, output=msg, reason=reason }) ->
                          if pendingPrefix `isPrefixOf` reason
                             then let pendingMsg = let s = drop (length pendingPrefix) reason
                                                   in take (length s - length pendingSuffix) s
                                  in quickCheckTestPending pendingMsg
                             else do let replay = "Replay argument: " ++ (show (show (Just (gen, size))))
                                     quickCheckTestFail (Just (adjustOutput msg ++ "\n" ++ replay))
                      Right (GaveUp { output=msg }) ->
                          quickCheckTestFail (Just (adjustOutput msg))
                      Right (NoExpectedFailure { output=msg }) ->
                          quickCheckTestFail (Just (adjustOutput msg))
                    return ()
    where
      pendingPrefix = "Exception: 'QCPendingException \""
      pendingSuffix = "\"'"
      adjustOutput s = trimTrailing $
          case s of
            '+':'+':'+':' ':'O':'K':',':' ':'p':rest -> 'P':rest
            '*':'*':'*':' ':'F':'a':'i':'l':'e':'d':'!':' ':rest -> rest
            '*':'*':'*':' ':rest -> rest
            _ -> s
      trimTrailing = reverse . dropWhile isSpace . reverse

data TestableWithQCArgs = forall a . Testable a =>
                          TestableWithQCArgs (Args -> Args) a

instance Testable TestableWithQCArgs where
    property (TestableWithQCArgs _ t) = property t

class WithQCArgs a where
    argsModifier :: a -> (Args -> Args)
    original :: a -> Maybe TestableWithQCArgs

instance WithQCArgs a where
    argsModifier _ = id
    original _ = Nothing

instance WithQCArgs TestableWithQCArgs where
    argsModifier (TestableWithQCArgs f _) = f
    original a = Just a

withQCArgs :: (WithQCArgs a, Testable a) => (Args -> Args) -> a
           -> TestableWithQCArgs
withQCArgs = TestableWithQCArgs

asTestableWithQCArgs :: (WithQCArgs a, Testable a) => a -> TestableWithQCArgs
asTestableWithQCArgs a =
    case original a of
      Just a' -> a'
      Nothing -> TestableWithQCArgs id a

qcPending :: Testable t => String -> t -> t
qcPending msg _ = throw (QCPendingException msg)
