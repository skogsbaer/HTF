{-# OPTIONS_GHC -F -pgmF dist/build/htfpp/htfpp #-}
module MaxPrevTime (maxPrevTimeMain) where

import Test.Framework
import Control.Concurrent

test_slow :: IO ()
test_slow = threadDelay 20000

test_fast :: IO ()
test_fast = return ()

maxPrevTimeMain args = htfMainWithArgs args htf_thisModulesTests
