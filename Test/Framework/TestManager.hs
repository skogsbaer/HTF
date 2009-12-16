-- 
-- Copyright (c) 2009   Stefan Wehr - http://www.stefanwehr.de
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

module Test.Framework.TestManager (

  TestID, Assertion, Test, TestSuite, Filter, FlatTest(..), TestSort(..),
  TestableHTF,

  quickCheckTestFail, quickCheckTestError,
  unitTestFail, blackBoxTestFail,

  makeQuickCheckTest, makeUnitTest, makeBlackBoxTest, makeTestSuite,
  makeAnonTestSuite,
  addToTestSuite, testSuiteAsTest,

  runTest, runTestWithArgs, runTestWithFilter

) where

import Control.Monad
import Control.Monad.State
import Data.List ( isInfixOf )

import qualified Test.HUnit.Lang as HU

import Test.Framework.Location ( Location, showLoc )
import Test.Framework.Utils ( readM )

type Assertion = IO ()

type TestID = String

assertFailureHTF :: String -> Assertion
-- Important: force the string argument, otherwise an error embedded
-- lazily inside the string might escape.
assertFailureHTF s = length s `seq` HU.assertFailure s

-- This is a HACK: we encode a custom error message for QuickCheck
-- failures and errors in a string, which is later parsed using read!

quickCheckTestError :: Maybe String -> Assertion
quickCheckTestError m = assertFailureHTF (show (False, m)) 

quickCheckTestFail :: Maybe String -> Assertion
quickCheckTestFail m = assertFailureHTF (show (True, m))

unitTestFail :: String -> IO a
unitTestFail s = 
    do assertFailureHTF s
       error "unitTestFail: UNREACHABLE"

blackBoxTestFail :: String -> Assertion
blackBoxTestFail = assertFailureHTF

makeQuickCheckTest :: TestID -> Location -> Assertion -> Test
makeQuickCheckTest id loc ass = BaseTest QuickCheckTest id (Just loc) ass

makeUnitTest :: TestID -> Location -> IO a -> Test
makeUnitTest id loc ass = BaseTest UnitTest id (Just loc) (ass >> return ())

makeBlackBoxTest :: TestID -> Assertion -> Test
makeBlackBoxTest id ass = BaseTest BlackBoxTest id Nothing ass

makeTestSuite :: TestID -> [Test] -> TestSuite
makeTestSuite = TestSuite

makeAnonTestSuite :: [Test] -> TestSuite
makeAnonTestSuite = AnonTestSuite

testSuiteAsTest :: TestSuite -> Test
testSuiteAsTest = CompoundTest

addToTestSuite :: TestSuite -> [Test] -> TestSuite
addToTestSuite (TestSuite id ts) ts' = TestSuite id (ts ++ ts')
addToTestSuite (AnonTestSuite ts) ts' = AnonTestSuite (ts ++ ts')

data TestSort = UnitTest | QuickCheckTest | BlackBoxTest
              deriving (Eq,Show,Read)

data Test = BaseTest TestSort TestID (Maybe Location) Assertion
          | CompoundTest TestSuite

data TestSuite = TestSuite TestID [Test]
               | AnonTestSuite [Test]

data FlatTest = FlatTest TestSort TestID (Maybe Location) Assertion

class TestableHTF t where
    flatten :: t -> [FlatTest]

instance TestableHTF Test where
    flatten = flattenTest Nothing

instance TestableHTF TestSuite where
    flatten = flattenTestSuite Nothing

type Path = Maybe String

flattenTest :: Path -> Test -> [FlatTest]
flattenTest path (BaseTest sort id mloc ass) = 
    [FlatTest sort (path `concatPath` id) mloc ass]
flattenTest path (CompoundTest ts) = 
    flattenTestSuite path ts

flattenTestSuite :: Path -> TestSuite -> [FlatTest]
flattenTestSuite path (TestSuite id ts) = 
    concatMap (flattenTest (Just (path `concatPath` id))) ts
flattenTestSuite path (AnonTestSuite ts) = 
    concatMap (flattenTest path) ts

concatPath :: Path -> String -> String
concatPath Nothing s = s
concatPath (Just s1) s2 = s1 ++ pathSep ++ s2
    where pathSep = ":"

data TestState = TestState { ts_passed :: Int
                           , ts_failed :: Int
                           , ts_error  :: Int }

initTestState :: TestState
initTestState = TestState 0 0 0

type TR = StateT TestState IO

runFlatTest :: FlatTest -> TR ()
runFlatTest (FlatTest sort id mloc ass) =
    do let name = id ++ case mloc of
                          Nothing -> ""
                          Just loc -> " (" ++ showLoc loc ++ ")"
       liftIO $ report name
       res <- liftIO $ HU.performTestCase ass
       case res of
         Nothing -> reportSuccess
         Just (isFailure', msg') ->
             let (isFailure, msg, doReport) = 
                     if sort /= QuickCheckTest
                        then (isFailure', msg', True)
                        else case readM msg' :: Maybe (Bool, Maybe String) of
                               Nothing ->
                                   error ("ERROR: " ++
                                          "Cannot deserialize QuickCheck " ++
                                          "error message " ++ show msg')
                               Just (b, ms) ->
                                   case ms of
                                     Nothing -> (b, "", False)
                                     Just s -> (b, s, True)
             in if isFailure
                   then do modify (\s -> s { ts_failed = 1 + (ts_failed s) })
                           when doReport $ reportFailure msg
                   else do modify (\s -> s { ts_error = 1 + (ts_error s) })
                           when doReport $ reportError msg
       liftIO $ report ""
    where
      reportSuccess = 
          do modify (\s -> s { ts_passed = 1 + (ts_passed s) })
             when (sort /= QuickCheckTest) $
                  liftIO $ report "+++ OK"
      reportFailure msg = 
          reportMessage msg failurePrefix
      reportError msg = 
          reportMessage msg errorPrefix
      reportMessage msg prefix = liftIO $ report (prefix ++ msg)
      failurePrefix = "*** Failed! "
      errorPrefix = "@@@ Error! "

runFlatTests :: [FlatTest] -> TR ()
runFlatTests = mapM_ runFlatTest

runTest :: TestableHTF t => t -> IO ()
runTest = runTestWithFilter (\_ -> True)

runTestWithArgs :: TestableHTF t => [String] -> t -> IO ()
runTestWithArgs [] = runTest
runTestWithArgs l = runTestWithFilter pred
    where pred (FlatTest _ id _ _) = any (\s -> s `isInfixOf` id) l

type Filter = FlatTest -> Bool

runTestWithFilter :: TestableHTF t => Filter -> t -> IO ()
runTestWithFilter pred t =
    do s <- execStateT (runFlatTests (filter pred (flatten t))) 
                       initTestState
       let total = ts_passed s + ts_failed s + ts_error s
       report ("* Tests:    " ++ show total ++ "\n" ++
               "* Passed:   " ++ show (ts_passed s) ++ "\n" ++
               "* Failures: " ++ show (ts_failed s) ++ "\n" ++
               "* Errors:   " ++ show (ts_error s))
       return ()

report :: String -> IO ()
report = putStrLn
