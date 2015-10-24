{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

-- The main test program.

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} MyPkg.A
import {-@ HTF_TESTS @-} MyPkg.B

main =
    do bbts <- blackBoxTests "bbt-dir" "../scripts/run-sample" ".num" defaultBBTArgs
       htfMain (htf_importedTests ++ [makeTestSuite "bbts" bbts])
