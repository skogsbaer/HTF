{-# OPTIONS_GHC -F -pgmF scripts/local-htfpp #-}
module SortByPrevTime (sortByPrevTimeMain) where

import Test.Framework
import Control.Concurrent

test_slow :: IO ()
test_slow = threadDelay 300000

test_fast :: IO ()
test_fast = return ()

sortByPrevTimeMain args = htfMainWithArgs args htf_thisModulesTests
