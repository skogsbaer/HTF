{-# OPTIONS_GHC -F -pgmF ./scripts/local-htfpp #-}
module Repeat (repeatMain) where

import Test.Framework

import Data.IORef
import System.IO.Unsafe

globalBool :: IORef Bool
globalBool = unsafePerformIO (newIORef True)
{-# NOINLINE globalBool #-}

readGlobalBool ::  IO Bool
readGlobalBool =
    do b <- readIORef globalBool
       writeIORef globalBool False
       return b

test_globalMVarIsTrue =
    do b <- readGlobalBool
       assertEqual b True

repeatMain args = htfMainWithArgs args htf_thisModulesTests