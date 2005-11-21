-- 
-- Copyright (c) 2005   Stefan Wehr - http://www.stefanwehr.de
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

data T = A | B deriving Eq

stringGap = "hello \
            \world!"

$(tests "assertTests" [d|

 test_stringGap = assertEqual stringGap "hello world!"

 test_assertEqual = assertEqual 1 2

 test_assertEqualNoShow = assertEqualNoShow A B

 test_assertSetEqual = assertSetEqual [1,2] [2]

 test_assertSetEqualSuccess = assertSetEqual [1,2] [2,1]

 test_assertNotNull = assertNotNull []

 test_assertNull = assertNull [1]

 test_assertThrows = assertThrows (return ()) (\_ -> True)

 test_assertThrows' = assertThrows (error "ERROR") (\_ -> False)

 test_someError = error "Bart Simpson!!"

 |])

$(tests "propTests" [d|

 prop_ok :: [Int] -> Property
 prop_ok xs = classify (null xs) "trivial" $ xs == (reverse (reverse xs))

 prop_fail :: [Int] -> Bool
 prop_fail xs = xs == (reverse xs)

 prop_exhaust = False ==> True

 prop_error :: Bool
 prop_error = error "Lisa"

 |])

mkCfg cfg = makeVerbose $ cfg { configMaxTest = 5 }

$(tests "propTestsVerbose" [d|

 prop_cfg_ok = (mkCfg, prop)
     where prop xs = classify (null xs) "trivial" $ 
                     xs == (reverse (reverse xs))
               where types = xs::[Int]

 prop_cfg_fail = (mkCfg, prop)
     where prop xs = xs == (reverse xs)
               where types = xs::[Int]

 prop_cfg_error :: (Config -> Config, Bool)
 prop_cfg_error = (mkCfg, error "Lisa")

 |])

allTests fbts = TestList $ [assertTests,propTests,propTestsVerbose] ++ fbts

main = 
    do fbts <- fileBasedTests "fbts" "fbt" "./run-fbt.sh" ".x" defaultFBTConfig
       runTestTT (allTests [fbts])