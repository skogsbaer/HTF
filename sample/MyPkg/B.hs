{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MyPkg.B (funB, htf_thisModulesTests) where

import Test.Framework

funB :: Int -> Int
funB x = x * 2

test_funB1 = assertEqual (funB 21) 42

test_funB2 = assertEqual (funB 0) 0
