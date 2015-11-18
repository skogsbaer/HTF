--
-- Copyright (c) 2013   Stefan Wehr - http://www.stefanwehr.de
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

import Test.Framework.History
import Test.Framework.Preprocessor
import Test.Framework.PrettyHaskell
import Test.Framework.HUnitWrapper
import Test.Framework.TestManager
import System.Exit
import Test.HUnit

allTests = historyTests ++ preprocessorTests ++ prettyHaskellTests ++ hunitWrapperTests ++ wrappableTests

main :: IO ()
main =
    do counts <- runTestTT hunitTests
       if errors counts /= 0 || failures counts /= 0
       then exitWith (ExitFailure 1)
       else exitWith ExitSuccess
    where
      hunitTests =
          TestList (map (\(name, code) -> TestLabel name (TestCase code)) allTests)
