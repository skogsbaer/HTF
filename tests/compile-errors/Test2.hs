{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
#include "Foo.h"
foo :: Int -> Int
foo i = i + "Stefan"

main :: IO ()
main = return ()
