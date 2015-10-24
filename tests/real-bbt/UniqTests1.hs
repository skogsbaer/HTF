{-# OPTIONS_GHC -F -pgmF scripts/local-htfpp #-}
module UniqTests1 (uniqTests1Main) where

import Test.Framework
import Control.Concurrent

test_fast :: IO ()
test_fast = return ()

uniqTests1Main args = htfMainWithArgs args htf_thisModulesTests
