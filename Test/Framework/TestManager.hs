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

  TestID, Assertion, Test,

  quickCheckTestFail, quickCheckTestError,
  unitTestFail, blackBoxTestFail,

  makeQuickCheckTest, makeUnitTest, makeBlackBoxTest, makeTestSuite,
  makeAnonTestSuite,

  runTest, runTestWithArgs, runTestWithFilter

) where

import Control.Monad.State
import Data.List ( isInfixOf )

import Test.HUnit.Lang (performTestCase, assertFailure)

type Assertion = IO ()

type TestID = String


-- This is a HACK: we encode a custom error message for QuickCheck
-- failures and errors in a string, which is later parsed using read!

quickCheckTestError :: Maybe String -> Assertion
quickCheckTestError m = assertFailure (show (False, m)) 

quickCheckTestFail :: Maybe String -> Assertion
quickCheckTestFail m = assertFailure (show (True, m))

unitTestFail :: String -> Assertion
unitTestFail = assertFailure

blackBoxTestFail :: String -> Assertion
blackBoxTestFail = assertFailure

makeQuickCheckTest :: TestID -> Assertion -> Test
makeQuickCheckTest = BaseTest QuickCheckTest

makeUnitTest :: TestID -> Assertion -> Test
makeUnitTest = BaseTest UnitTest

makeBlackBoxTest :: TestID -> Assertion -> Test
makeBlackBoxTest = BaseTest BlackBoxTest

makeTestSuite :: TestID -> [Test] -> Test
makeTestSuite = TestSuite

makeAnonTestSuite :: [Test] -> Test
makeAnonTestSuite = AnonTestSuite

data TestSort = UnitTest | QuickCheckTest | BlackBoxTest
              deriving (Eq,Show,Read)

data Test = BaseTest TestSort TestID Assertion
          | TestSuite TestID [Test]
          | AnonTestSuite [Test]

data FlatTest = FlatTest TestSort TestID Assertion

flattenTest :: Test -> [FlatTest]
flattenTest = flattenTest' Nothing
    where
      flattenTest' path (BaseTest sort id ass) = 
          [FlatTest sort (path `concatP` id) ass]
      flattenTest' path (TestSuite id ts) = 
          concatMap (flattenTest' (Just (path `concatP` id))) ts
      flattenTest' path (AnonTestSuite ts) = 
          concatMap (flattenTest' path) ts
      concatP Nothing s = s
      concatP (Just s1) s2 = s1 ++ pathSep ++ s2
      pathSep = ":"

data TestState = TestState { ts_passed :: Int
                           , ts_failed :: Int
                           , ts_error  :: Int }

initTestState :: TestState
initTestState = TestState 0 0 0

type TR = StateT TestState IO

runFlatTest :: FlatTest -> TR ()
runFlatTest (FlatTest sort id ass) =
    do liftIO $ report id
       res <- liftIO $ performTestCase ass
       case res of
         Nothing -> reportSuccess
         Just (isFailure', msg') ->
             let (isFailure, msg, doReport) = 
                     if sort /= QuickCheckTest
                        then (isFailure', msg', True)
                        else let (b, ms) = (read msg' :: (Bool, Maybe String))
                             in case ms of
                                  Nothing -> (b, "", False)
                                  Just s -> (b, s, True)
             in if doReport
                   then if isFailure then reportFailure msg else reportError msg
                   else return ()
       liftIO $ report ""
    where
      reportSuccess = 
          do modify (\s -> s { ts_passed = 1 + (ts_passed s) })
             when (sort /= QuickCheckTest) $
                  liftIO $ report "+++ OK"
      reportFailure msg = 
          do modify (\s -> s { ts_failed = 1 + (ts_failed s) })
             reportMessage msg failurePrefix
      reportError msg = 
          do modify (\s -> s { ts_error = 1 + (ts_error s) })
             reportMessage msg errorPrefix
      reportMessage msg prefix = liftIO $ report (prefix ++ msg)
      failurePrefix = "*** Failed! "
      errorPrefix = "@@@ Error! "

runFlatTests :: [FlatTest] -> TR ()
runFlatTests = mapM_ runFlatTest

runTest :: Test -> IO ()
runTest = runTestWithFilter (\_ -> True)

runTestWithArgs :: [String] -> Test -> IO ()
runTestWithArgs [] = runTest
runTestWithArgs l = runTestWithFilter pred
    where pred (FlatTest _ id _) = any (\s -> s `isInfixOf` id) l

type Filter = FlatTest -> Bool

runTestWithFilter :: Filter -> Test -> IO ()
runTestWithFilter pred t =
    do s <- execStateT (runFlatTests (filter pred (flattenTest t))) initTestState
       let total = ts_passed s + ts_failed s + ts_error s
       report ("* Tests:    " ++ show total ++ "\n" ++
               "* Passed:   " ++ show (ts_passed s) ++ "\n" ++
               "* Failures: " ++ show (ts_failed s) ++ "\n" ++
               "* Errors:   " ++ show (ts_error s))
       return ()

report :: String -> IO ()
report = putStrLn
