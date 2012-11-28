{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

-- The main test program.

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} MyPkg.A
import {-@ HTF_TESTS @-} MyPkg.B

main =
    do bbts <- blackBoxTests "bbt-dir" "dist/build/sample/sample" ".num" defaultBBTArgs
       htfMain (htf_importedTests ++ [makeTestSuite "bbts" bbts])
