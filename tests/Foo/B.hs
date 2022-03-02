{-# OPTIONS_GHC -F -pgmF ./scripts/local-htfpp #-}

module Foo.B (htf_thisModulesTests) where

import qualified Test.Framework as HTF

test_b_OK = HTF.assertEqual 1 1
