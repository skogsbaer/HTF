{-# OPTIONS_GHC -F -pgmF ../../scripts/local-htfpp #-}

import Test.Framework
#include "Foo.h"

foo :: Int -> Int
foo i = i + "Stefan"

main :: IO ()
main = return ()
