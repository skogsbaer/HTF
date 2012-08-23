{-# OPTIONS_GHC -F -pgmF htfpp #-}
--
-- Copyright (c) 2005,2010   Stefan Wehr - http://www.stefanwehr.de
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--

import Test.Framework
import System.Environment
import Control.Exception
import {-@ HTF_TESTS @-} qualified TestHTFHunitBackwardsCompatible
import {-@ HTF_TESTS @-} qualified Foo.A as A
import {-@ HTF_TESTS @-} Foo.B

data T = A | B
       deriving Eq

{-
stringGap = "hello \
            \world!"
-}
stringGap = "hello world!"

handleExc :: a -> SomeException -> a
handleExc x _ = x

test_stringGap = assertEqual stringGap "hello world!"

test_assertEqual = assertEqual 1 2

test_assertEqualV = assertEqualVerbose "blub" 1 2

test_assertEqualNoShow = assertEqualNoShow A B

test_assertListsEqualAsSets = assertListsEqualAsSets [1,2] [2]

test_assertSetEqualSuccess = assertListsEqualAsSets [1,2] [2,1]

test_assertNotEmpty = assertNotEmpty []

test_assertEmpty = assertEmpty [1]

test_assertThrows = assertThrows (return () :: IO ()) (handleExc True)

test_assertThrows' = assertThrows (error "ERROR") (handleExc False)

test_someError = error "Bart Simpson!!"

test_pendingTest = unitTestPending "This test is pending"

data Expr = PlusExpr Expr Expr
          | MultExpr Expr Expr
          | Literal Int
          | Variable String
            deriving (Eq, Show)

test_diff = assertEqual (mkExpr 1) (mkExpr 2)
    where
      mkExpr i =
          PlusExpr (PlusExpr (MultExpr (PlusExpr (Variable "foo")
                                                     (MultExpr (Literal 42) (Variable "bar")))
                                       (PlusExpr (Literal i) (Literal 2)))
                             (Literal 581))
                   (Variable "egg")

prop_ok :: [Int] -> Property
prop_ok xs = classify (null xs) "trivial" $ xs == (reverse (reverse xs))

prop_fail :: [Int] -> Bool
prop_fail xs = xs == (reverse xs)

prop_pendingProp :: Int -> Bool
prop_pendingProp x = qcPending "This property is pending" (x == 0)

prop_exhaust = False ==> True

prop_error :: Bool
prop_error = error "Lisa"

changeArgs args = args { maxSuccess = 1 }

prop_ok' = withQCArgs (\a -> a { maxSuccess = 1}) $
                     \xs -> classify (null xs) "trivial" $
                            (xs::[Int]) == (reverse (reverse xs))

prop_fail' =
    withQCArgs (\a -> a { replay = read "Just (1292732529 652912053,3)" }) prop
    where prop xs = xs == (reverse xs)
              where types = xs::[Int]

prop_error' :: TestableWithQCArgs
prop_error' = withQCArgs changeArgs $ (error "Lisa" :: Bool)

main =
    do args <- getArgs
       bbts <- blackBoxTests "bbt" "./run-bbt.sh" ".x"
                 (defaultBBTArgs { bbtArgs_verbose = False })
       runTestWithArgs args ([addToTestSuite htf_thisModulesTests bbts] ++
                             htf_importedTests)
