{-# LANGUAGE FlexibleInstances,OverlappingInstances,ExistentialQuantification #-}

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

  TestableWithArgs, withArgs, asTestableWithArgs

) where

import qualified Data.Map as Map
import Control.Concurrent.MVar
import Prelude hiding ( catch )
import Control.Exception ( throw, catch, SomeException )
import System.IO
import System.IO.Unsafe
import System.Random
import Data.List( group, sort, intersperse )
import Data.Char

import Test.QuickCheck
import Test.QuickCheck.Property

import Test.Framework.TestManager

data QCState = QCState { qc_args :: Args }
             
qcState :: MVar QCState
qcState = unsafePerformIO (newMVar (QCState defaultArgs))
{-# NOINLINE qcState #-}

defaultArgs :: Args
defaultArgs = stdArgs

setDefaultArgs :: Args -> IO ()
setDefaultArgs args = 
    do withMVar qcState $ \state -> return (state { qc_args = args })
       return ()

getCurrentArgs :: IO Args
getCurrentArgs = 
    withMVar qcState $ \state -> return (qc_args state)

testableAsAssertion :: (Testable t, WithArgs t) => t -> Assertion
testableAsAssertion t = 
    withMVar qcState $ \state ->
        do eitherArgs <- 
               (let a = (argsModifier t) (qc_args state)
                in length (show a) `seq` return (Right a))
               `catch`
               (\e -> return $ Left (show (e :: SomeException)))
           case eitherArgs of
             Left err -> quickCheckTestError
                            (Just ("Cannot evaluate custom arguments: " ++ err))
             Right args ->
                 do res <- quickCheckWithResult args t
                    case res of
                      Success _ -> return ()
                      Failure gen size reason _ -> 
                           do putStrLn ("Replay argument: " ++
                                        (show (show (Just (gen, size)))))
                              quickCheckTestFail Nothing
                      _ -> quickCheckTestFail Nothing
                    return ()

data TestableWithArgs = forall a . Testable a => 
                        TestableWithArgs (Args -> Args) a

instance Testable TestableWithArgs where
    property (TestableWithArgs _ t) = property t

class WithArgs a where
    argsModifier :: a -> (Args -> Args)
    original :: a -> Maybe TestableWithArgs

instance WithArgs a where
    argsModifier _ = id
    original _ = Nothing

instance WithArgs TestableWithArgs where
    argsModifier (TestableWithArgs f _) = f
    original a = Just a

withArgs :: (WithArgs a, Testable a) => (Args -> Args) -> a -> TestableWithArgs
withArgs = TestableWithArgs

asTestableWithArgs :: (WithArgs a, Testable a) => a -> TestableWithArgs
asTestableWithArgs a = 
    case original a of
      Just a' -> a'
      Nothing -> TestableWithArgs id a
