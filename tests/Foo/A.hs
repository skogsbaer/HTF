{-# OPTIONS_GHC -F -pgmF ./scripts/local-htfpp #-}
{-# LANGUAGE CPP #-}

module Foo.A where

import Test.Framework

#include "test.h"

test_a_FAIL =
    assertEqual x y
