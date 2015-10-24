{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -F -pgmF scripts/local-htfpp #-}
module UniqTests2 (uniqTests2Main) where

import Test.Framework
import Control.Concurrent

test_fast :: IO ()
test_fast =
    let l =
            flip map [True, False] $ \case
                                        True -> 1
                                        False -> 2
    in putStrLn (show l)

uniqTests2Main args = htfMainWithArgs args htf_thisModulesTests
