{-# OPTIONS_GHC -F -pgmF scripts/local-htfpp #-}
module PrevFactor (prevFactorMain) where

import Test.Framework
import Test.Framework.TestManager
import System.Environment
import System.Exit
import Control.Concurrent

doTest :: Bool -> IO ()
doTest slow =
    if slow then threadDelay 100000 else threadDelay 1000

prevFactorMain args =
    do ecode <-
           case args of
             "SLOW":rest -> runTestWithArgs rest [doTest True]
             _ -> runTestWithArgs args [doTest False]
       exitWith ecode
