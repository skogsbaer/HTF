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

  makeQuickCheckTest, makeUnitTest, makeBlackBoxTest, makeTestSuite,
  makeAnonTestSuite,
  addToTestSuite, testSuiteAsTest,

  runTest, runTestWithArgs, runTestWithFilter

) where

import Control.Monad
import Control.Monad.State
import System.Exit (ExitCode(..))
import Data.List ( isInfixOf, isPrefixOf, partition )
import Text.PrettyPrint
import qualified Data.List as List

import qualified Test.HUnit.Lang as HU

import Test.Framework.Location ( Location, showLoc )
import Test.Framework.Utils ( readM )
import {-# SOURCE #-} Test.Framework.TestManagerInternal

type Assertion = IO ()

type TestID = String

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

instance TestableHTF t => TestableHTF [t] where
    flatten = concatMap flatten

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

data TestState = TestState { ts_passed  :: [String]
                           , ts_failed  :: [String]
                           , ts_error   :: [String]
                           , ts_pending :: [String] }

initTestState :: TestState
initTestState = TestState [] [] [] []

type TR = StateT TestState IO

runFlatTest :: FlatTest -> TR ()
runFlatTest (FlatTest sort id mloc ass) =
    do let name = id ++ case mloc of
                          Nothing -> ""
                          Just loc -> " (" ++ showLoc loc ++ ")"
       liftIO $ report name
       res <- liftIO $ HU.performTestCase ass
       case res of
         Nothing -> reportSuccess name
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
                                     Nothing -> (b, "", True)
                                     Just s -> (b, s, True)
             in if isFailure
                   then case extractPendingMessage msg of
                          Nothing -> do modify (\s -> s { ts_failed =
                                                             name : (ts_failed s) })
                                        when doReport $ reportFailure msg
                          Just msg -> do modify (\s -> s { ts_pending =
                                                             name : (ts_pending s) })
                                         when doReport $ reportPending msg
                   else do modify (\s -> s { ts_error =
                                             name : (ts_error s) })
                           when doReport $ reportError msg
       liftIO $ report ""
    where
      reportSuccess name =
          do modify (\s -> s { ts_passed = name : (ts_passed s) })
             liftIO $ report "+++ OK"
      reportPending msg =
          reportMessage msg pendingPrefix
      reportFailure msg =
          reportMessage msg failurePrefix
      reportError msg =
          reportMessage msg errorPrefix
      reportMessage msg prefix = liftIO $ report (prefix ++ msg)
      failurePrefix = "*** Failed! "
      errorPrefix = "@@@ Error! "
      pendingPrefix = "^^^ Pending! "

runFlatTests :: [FlatTest] -> TR ()
runFlatTests = mapM_ runFlatTest

runTest :: TestableHTF t => t -> IO ExitCode
runTest = runTestWithFilter (\_ -> True)

runTestWithArgs :: TestableHTF t => [String] -> t -> IO ExitCode
runTestWithArgs l tests =
    let (pos, neg') = partition (\x -> not $ "-" `isPrefixOf` x) l
        neg = map tail neg'
    in runTestWithFilter (pred pos neg) tests
    where
      pred pos neg (FlatTest _ id _ _) =
          if (any (\s -> s `isInfixOf` id) neg)
             then False
             else null pos || any (\s -> s `isInfixOf` id) pos

type Filter = FlatTest -> Bool

runTestWithFilter :: TestableHTF t => Filter -> t -> IO ExitCode
runTestWithFilter pred t =
    do s <- execStateT (runFlatTests (filter pred (flatten t)))
                       initTestState
       let passed = length (ts_passed s)
           pending = length (ts_pending s)
           failed = length (ts_failed s)
           error = length (ts_error s)
           total = passed + failed + error + pending
       report ("* Tests:    " ++ show total ++ "\n" ++
               "* Passed:   " ++ show passed ++ "\n" ++
               "* Pending:  " ++ show pending ++ "\n" ++
               "* Failures: " ++ show failed ++ "\n" ++
               "* Errors:   " ++ show error )
       when (pending > 0) $
          reportDoc (text "\nPending:" $$ renderTestNames
                                             (reverse (ts_pending s)))
       when (failed > 0) $
          reportDoc (text "\nFailures:" $$ renderTestNames
                                             (reverse (ts_failed s)))
       when (error > 0) $
          reportDoc (text "\nErrors:" $$ renderTestNames
                                             (reverse (ts_error s)))
       return $ case () of
                  _| failed == 0 && error == 0 -> ExitSuccess
                   | error == 0                -> ExitFailure 1
                   | otherwise                 -> ExitFailure 2
    where
      renderTestNames l =
          nest 2 (vcat (map (\name -> text "*" <+> text name) l))

reportDoc :: Doc -> IO ()
reportDoc doc = report (render doc)
