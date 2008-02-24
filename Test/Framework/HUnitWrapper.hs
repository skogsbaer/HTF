--
-- Copyright (c) 2005   Stefan Wehr - http://www.stefanwehr.de
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA
--

module Test.Framework.HUnitWrapper (

  HU.Assertion,

  assertBool_, assertEqual_, assertEqualNoShow_, assertNotNull_, assertNull_,
  assertSetEqual_, assertThrows_,

  assertFailure,

  HU.Test(..), runTestTT,

  joinPathElems, showPathLabel

) where

import System.IO ( stderr )
import Data.List ( (\\) )
import Control.Exception
import Data.Version
import qualified Test.HUnit as HU

import Test.Framework.Location
import Test.Framework.Configuration

--
-- Assertions
--

-- WARNING: do not forget to add a preprocessor macro for new assertions!!

assertFailure :: String -> IO ()
assertFailure s =
    if ghcVersion <= buggyVersion
       then -- because of a bug in HUnit shipped with GHC 6.4.1, we must
            -- throw an exception e such that (show e) does not have a prefix
            -- like `user error'.
            error (hunitPrefix ++ s)
       else HU.assertFailure s
    where hunitPrefix = "HUnit:"
          buggyVersion = Version [6,4,1] []

assertBool_ :: Location -> Bool -> HU.Assertion
assertBool_ loc False = assertFailure ("assert failed at " ++ showLoc loc)
assertBool_ loc True = return ()

assertEqual_ :: (Eq a, Show a) => Location -> a -> a -> HU.Assertion
assertEqual_ loc expected actual =
    if expected /= actual
       then assertFailure msg
       else return ()
    where msg = "assertEqual failed at " ++ showLoc loc ++
                "\n expected: " ++ show expected ++ "\n but got:  " ++ show actual

assertEqualNoShow_ :: Eq a => Location -> a -> a -> HU.Assertion
assertEqualNoShow_ loc expected actual =
    if expected /= actual
       then assertFailure ("assertEqualNoShow failed at " ++ showLoc loc)
       else return ()

assertSetEqual_ :: (Eq a, Show a) => Location -> [a] -> [a] -> HU.Assertion
assertSetEqual_ loc expected actual =
    let ne = length expected
        na = length actual
        in case () of
            _| ne /= na ->
                 assertFailure ("assertSetEqual failed at " ++ showLoc loc
                                ++ "\n expected length: " ++ show ne
                                ++ "\n actual length: " ++ show na)
             | not (unorderedEq expected actual) ->
                 assertFailure ("assertSetEqual failed at " ++ showLoc loc
                                ++ "\n expected: " ++ show expected
                                ++ "\n actual: " ++ show actual)
             | otherwise -> return ()
    where unorderedEq l1 l2 =
              null (l1 \\ l2) && null (l2 \\ l1)


assertNotNull_ :: Location -> [a] -> HU.Assertion
assertNotNull_ loc [] = assertFailure ("assertNotNull failed at " ++ showLoc loc)
assertNotNull_ _ (_:_) = return ()

assertNull_ :: Location -> [a] -> HU.Assertion
assertNull_ loc (_:_) = assertFailure ("assertNull failed at " ++ showLoc loc)
assertNull_ loc [] = return ()

assertThrows_ :: Location -> IO a -> (Exception -> Bool) -> HU.Assertion
assertThrows_ loc io f =
    do res <- try io
       case res of
         Right _ -> assertFailure ("assertThrows failed at " ++ showLoc loc ++
                                   ": no exception was thrown")
         Left e -> if f e then return ()
                   else assertFailure ("assertThrows failed at " ++
                                       showLoc loc ++
                                       ": wrong exception was thrown: " ++
                                       show e)

--
-- Test runner
--

{-
We use our own test runner because HUnit print test paths a bit unreadable:
If a test list contains a named tests, then HUnit prints `i:n' where i
is the index of the named tests and n is the name.
-}

{-
`runTestText` executes a test, processing each report line according
to the given reporting scheme.  The reporting scheme's state is
threaded through calls to the reporting scheme's function and finally
returned, along with final count values.
-}

runTestText :: HU.PutText st -> HU.Test -> IO (HU.Counts, st)
runTestText (HU.PutText put us) t = do
  put allTestsStr True us
  (counts, us') <- HU.performTest reportStart reportError reportFailure us t
  us'' <- put (HU.showCounts counts) True us'
  return (counts, us'')
 where
  allTestsStr = unlines ("All tests:" :
                         map (\p -> "  " ++ showPath p) (HU.testCasePaths t))
  reportStart ss us = put (HU.showCounts (HU.counts ss)) False us
  reportError   = reportProblem "Error:"   "Error in:   "
  reportFailure = reportProblem "Failure:" "Failure in: "
  reportProblem p0 p1 msg ss us = put line True us
   where line  = "### " ++ kind ++ path' ++ '\n' : msg ++ "\n"
         kind  = if null path' then p0 else p1
         path' = showPath (HU.path ss)



{-
`showPath` converts a test case path to a string, separating adjacent
elements by ':'.  An element of the path is quoted (as with `show`)
when there is potential ambiguity.
-}

showPath :: HU.Path -> String
showPath [] = ""
showPath nodes = foldr1 joinPathElems
                   (map showNode (filterNodes (reverse nodes)))
 where showNode (HU.ListItem n) = show n
       showNode (HU.Label label) = showPathLabel label
       filterNodes (HU.ListItem _ : l@(HU.Label _) : rest) =
           l : filterNodes rest
       filterNodes [] = []
       filterNodes (x:rest) = x : filterNodes rest

joinPathElems :: String -> String -> String
joinPathElems s1 s2 = s1 ++ ":" ++ s2

showPathLabel :: String -> String
showPathLabel s =
    let ss = show s
        in if ':' `elem` s || "\"" ++ s ++ "\"" /= ss then ss else s

{-
`runTestTT` provides the "standard" text-based test controller.
Reporting is made to standard error, and progress reports are
included.  For possible programmatic use, the final counts are
returned.  The "TT" in the name suggests "Text-based reporting to the
Terminal".
-}

runTestTT :: HU.Test -> IO HU.Counts
runTestTT t = do (counts, _) <- runTestText (HU.putTextToHandle stderr False) t
                 return counts
