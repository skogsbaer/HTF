{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MyPkg.A (funA, htf_thisModulesTests) where

import Test.Framework

funA :: Int -> Int
funA x = x + 1

test_funA1 = assertEqual (funA 41) 42

test_funA2 = assertEqual (funA 2) 3
