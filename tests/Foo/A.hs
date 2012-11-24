{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE CPP #-}

module Foo.A where

import Test.Framework

#include "test.h"

test_a =
    assertEqual x y
