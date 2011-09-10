--
-- Copyright (c) 2009-2011   Stefan Wehr - http://www.stefanwehr.de
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

  parseTestArgs, runTest, runTestWithArgs, runTestWithOptions

) where

import Control.Monad
import Control.Monad.RWS
import System.Exit (ExitCode(..))
import Data.List ( isInfixOf, isPrefixOf, partition )
import Text.PrettyPrint
import qualified Data.List as List

import System.Directory (getTemporaryDirectory)
import System.IO
import System.Console.GetOpt
import GHC.IO.Handle

import qualified Test.HUnit.Lang as HU

import Test.Framework.Location ( Location, showLoc )
import Test.Framework.Utils ( readM, ensureNewline )
import {-# SOURCE #-} Test.Framework.TestManagerInternal
import Test.Framework.TestConfig
import Test.Framework.Colors

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

type TR = RWST TestConfig () TestState IO

data HandleRedirection = HandleRedirection { hr_original :: Handle
                                           , hr_originalCopy :: Handle
                                           , hr_newHandle :: Handle
                                           , hr_newFilePath :: FilePath }

redirectHandle :: String -> Handle -> IO HandleRedirection
redirectHandle nameTmpl h =
    do tmpDir <- getTemporaryDirectory
       (path, new) <- openBinaryTempFile tmpDir nameTmpl
       copy <- hDuplicate h
       hDuplicateTo new h
       return $ HandleRedirection { hr_original = h
                                  , hr_originalCopy = copy
                                  , hr_newHandle = new
                                  , hr_newFilePath = path }

unredirectHandle :: Bool -> HandleRedirection -> IO ()
unredirectHandle printOutput hr =
    do hClose (hr_newHandle hr)
       hDuplicateTo (hr_originalCopy hr) (hr_original hr) -- restore
       when (printOutput) $ do x <- readFile (hr_newFilePath hr)
                               hPutStr (hr_original hr) x

runFlatTest :: FlatTest -> TR ()
runFlatTest (FlatTest sort id mloc ass) =
    do let name = id ++ case mloc of
                          Nothing -> ""
                          Just loc -> " (" ++ showLoc loc ++ ")"
       x <- atStart name
       res <- liftIO $ HU.performTestCase ass
       let (testResult, msg) =
             case res of
               Nothing -> (Pass, "")
               Just (isFailure, msg') ->
                   if sort /= QuickCheckTest
                      then if isFailure
                              then case extractPendingMessage msg' of
                                     Nothing -> (Fail, msg')
                                     Just msg'' -> (Pending, msg'')
                              else (Error, msg')
                      else case readM msg' :: Maybe (TestResult, Maybe String) of
                             Nothing ->
                                 error ("ERROR: " ++
                                        "Cannot deserialize QuickCheck " ++
                                        "error message " ++ show msg')
                             Just (r, ms) ->
                                 case ms of
                                   Nothing -> (r, "")
                                   Just s -> (r, s)
       afterRunning x name testResult
       case testResult of
         Pass -> reportSuccess name msg
         Pending ->
             do modify (\s -> s { ts_pending = name : (ts_pending s) })
                reportPending msg
         Fail ->
             do modify (\s -> s { ts_failed = name : (ts_failed s) })
                reportFailure msg
         Error ->
             do modify (\s -> s { ts_error = name : (ts_error s) })
                reportError msg
       atEnd testResult
    where
      testStartMessage name =
          do t <- colorize testStartColor "[TEST] "
             return $ t ++ name
      atStart name =
          do tc <- ask
             if tc_quiet tc
                then liftIO $
                     do tmpDir <- getTemporaryDirectory
                        stdoutRedir <- redirectHandle "HTF.out" stdout
                        stderrRedir <- redirectHandle "HTF.err" stderr
                        return $ Just (stdoutRedir, stderrRedir)
                else do msg <- liftIO $ testStartMessage name
                        reportTR Debug msg
                        return Nothing
      afterRunning x name testResult =
          do tc <- ask
             if tc_quiet tc
                then case x of
                       Just (stdoutRedir, stderrRedir) -> liftIO $
                          do let printOutput = needsReport testResult
                             when printOutput $
                                  do msg <- testStartMessage name
                                     report tc Info msg
                             unredirectHandle printOutput stderrRedir
                             unredirectHandle printOutput stdoutRedir
                else return ()
      atEnd testResult =
          do tc <- ask
             if not (tc_quiet tc) || needsReport testResult
                then reportTR Info ""
                else return ()
      needsReport testResult = testResult `elem` [Fail, Error, Pending]
      reportSuccess name msg =
          do modify (\s -> s { ts_passed = name : (ts_passed s) })
             pref <- okPrefix
             reportTR Debug (ensureNewline msg ++ pref)
      reportPending msg =
          do pref <- pendingPrefix
             reportMessage Info msg  pref
      reportFailure msg =
          do pref <- failurePrefix
             reportMessage Info msg pref
      reportError msg =
          do pref <- errorPrefix
             reportMessage Info msg pref
      reportMessage isImportant msg prefix =
          reportTR isImportant (ensureNewline msg ++ prefix)
      failurePrefix = liftIO $ colorize warningColor "*** Failed! "
      errorPrefix = liftIO $ colorize warningColor "@@@ Error! "
      pendingPrefix = liftIO $ colorize pendingColor "^^^ Pending! "
      okPrefix = liftIO $ colorize testOkColor  "+++ OK"

runFlatTests :: [FlatTest] -> TR ()
runFlatTests = mapM_ runFlatTest

runTest :: TestableHTF t => t -> IO ExitCode
runTest = runTestWithOptions defaultTestOptions

optionDescriptions :: [OptDescr (TestOptions -> TestOptions)]
optionDescriptions =
    [ Option ['v']     ["verbose"] (NoArg (\o -> o { opts_quiet = False })) "chatty output"
    , Option ['q']     ["quiet"]   (NoArg (\o -> o { opts_quiet = True })) "only display errors"
    , Option ['h']     ["help"]    (NoArg (\o -> o { opts_help = True })) "display this message"
    ]

runTestWithArgs :: TestableHTF t => [String] -> t -> IO ExitCode
runTestWithArgs args t =
    case parseTestArgs args of
      Left err ->
          do hPutStrLn stderr err
             return $ ExitFailure 1
      Right opts ->
          runTestWithOptions opts t

parseTestArgs :: [String] -> Either String TestOptions
parseTestArgs args =
    case getOpt Permute optionDescriptions args of
      (optTrans, tests, []  ) ->
          let (pos, neg') = partition (\x -> not $ "-" `isPrefixOf` x) tests
              neg = map tail neg'
              pred (FlatTest _ id _ _) =
                  if (any (\s -> s `isInfixOf` id) neg)
                     then False
                     else null pos || any (\s -> s `isInfixOf` id) pos
              opts = (foldr ($) defaultTestOptions optTrans) { opts_filter = pred }
          in Right opts
      (_,_,errs) ->
          Left (concat errs ++ usageInfo usageHeader optionDescriptions)

usageHeader :: String
usageHeader = "USAGE: COMMAND [OPTION ...] TEST_NAME ...\n"

type Filter = FlatTest -> Bool

data TestOptions = TestOptions {
      opts_quiet :: Bool
    , opts_filter :: Filter
    , opts_help :: Bool
    }

defaultTestOptions = TestOptions {
      opts_quiet = tc_quiet defaultTestConfig
    , opts_filter = const True
    , opts_help = False
    }

runTestWithOptions :: TestableHTF t => TestOptions -> t -> IO ExitCode
runTestWithOptions opts t =
    if opts_help opts
       then do hPutStrLn stderr (usageInfo usageHeader optionDescriptions)
               return $ ExitFailure 1
       else
         do let pred = opts_filter opts
                tc = optsToConfig opts
            (_, s, _) <- runRWST (runFlatTests (filter pred (flatten t))) tc initTestState
            let passed = length (ts_passed s)
                pending = length (ts_pending s)
                failed = length (ts_failed s)
                error = length (ts_error s)
                total = passed + failed + error + pending
            pendings <- colorize pendingColor "* Pending:"
            failures <- colorize warningColor "* Failures:"
            errors <- colorize warningColor "* Errors:"
            report tc Info ("* Tests:    " ++ show total ++ "\n" ++
                            "* Passed:   " ++ show passed ++ "\n" ++
                            pendings ++ "  " ++ show pending ++ "\n" ++
                            failures ++ " " ++ show failed ++ "\n" ++
                            errors ++ "   " ++ show error )
            when (pending > 0) $
               reportDoc tc Info
                   (text ('\n' : pendings) $$ renderTestNames (reverse (ts_pending s)))
            when (failed > 0) $
               reportDoc tc Info
                   (text ('\n' : failures) $$ renderTestNames (reverse (ts_failed s)))
            when (error > 0) $
               reportDoc tc Info
                   (text ('\n' : errors) $$ renderTestNames (reverse (ts_error s)))
            return $ case () of
                       _| failed == 0 && error == 0 -> ExitSuccess
                        | error == 0                -> ExitFailure 1
                        | otherwise                 -> ExitFailure 2
    where
      renderTestNames l =
          nest 2 (vcat (map (\name -> text "*" <+> text name) l))
      optsToConfig opts =
          TestConfig { tc_quiet = opts_quiet opts }

reportDoc :: TestConfig -> ReportLevel -> Doc -> IO ()
reportDoc tc level doc = report tc level (render doc)

reportTR :: ReportLevel -> String -> TR ()
reportTR level msg =
    do tc <- ask
       liftIO $ report tc level msg
