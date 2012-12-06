{-# OPTIONS_GHC -F -pgmF ./dist/build/htfpp/htfpp -optF --hunit #-}
module TestHTFHunitBackwardsCompatible where

import Test.Framework

test_1 = do assertEqual "1 == 2" 1 2
            assertEqualHTF 1 1

test_2 = do assertJust "foo" (Just 1)
            assertJustHTF (Just 1)
