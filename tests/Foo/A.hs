{-# OPTIONS_GHC -F -pgmF ./dist/build/htfpp/htfpp #-}
{-# LANGUAGE CPP #-}

module Foo.A where

import Test.Framework

#include "test.h"

test_a_FAIL =
    assertEqual x y
