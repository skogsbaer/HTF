{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MyPkg.C (htf_importedTests) where

import Test.Framework
import {-@ HTF_TESTS @-} MyPkg.A
import {-@ HTF_TESTS @-} MyPkg.B
