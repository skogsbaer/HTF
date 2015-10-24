{-# OPTIONS_GHC -F -pgmF scripts/local-htfpp #-}
module MaxCurTime (maxCurTimeMain) where

import Test.Framework
import Control.Concurrent
import System.IO.Unsafe

test_slow :: IO ()
test_slow = threadDelay 20000

prop_verySlow :: Int -> Bool
prop_verySlow _i = unsafePerformIO test_slow `seq` True

maxCurTimeMain args = htfMainWithArgs args htf_thisModulesTests
