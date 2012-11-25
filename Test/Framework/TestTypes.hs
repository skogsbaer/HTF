{-| 

This module defines types (and small auxiliary functions)
for organizing tests, for configuring the execution of
tests, and for representing and reporting their results.

-}
module Test.Framework.TestTypes (

  -- * Organizing tests
  TestID, Assertion, Test(..), TestSuite(..), TestSort(..),
  TestPath(..), GenFlatTest(..), FlatTest, TestFilter,
  testPathToList, flatName,

  -- * Executing tests
  TR, TestState(..), initTestState, TestConfig(..), TestOutput(..),

  -- * Reporting results
  ReportAllTests, ReportGlobalStart, ReportTestStart, ReportTestResult, ReportGlobalResults,
  TestReporter(..),

  -- * Specifying results.
  TestResult(..), FlatTestResult, Milliseconds, RunResult(..)

) where

import Test.Framework.Location

import Control.Monad.RWS
import System.IO
import Data.Maybe
import qualified Data.List as List

-- | An assertion is just an 'IO' action.
type Assertion = IO ()

-- | Type for naming tests.
type TestID = String

-- | Type for distinguishing different sorts of tests.
data TestSort = UnitTest | QuickCheckTest | BlackBoxTest
              deriving (Eq,Show,Read)

-- | Abstract type for tests and their results.
data Test = BaseTest TestSort TestID (Maybe Location) Assertion
          | CompoundTest TestSuite

-- | Abstract type for test suites and their results.
data TestSuite = TestSuite TestID [Test]
               | AnonTestSuite [Test]

-- | A type denoting the hierarchical name of a test.
data TestPath = TestPathBase TestID
              | TestPathCompound (Maybe TestID) TestPath

-- | Splits a 'TestPath' into a list of test identifiers.
testPathToList :: TestPath -> [Maybe TestID]
testPathToList (TestPathBase i) = [Just i]
testPathToList (TestPathCompound mi p) =
    mi : testPathToList p

-- | Creates a string representation from a 'TestPath'.
flatName :: TestPath -> String
flatName p =
    List.intercalate ":" (map (fromMaybe "") (testPathToList p))

-- | Generic type for flattened tests and their results.
data GenFlatTest a
    = FlatTest
      { ft_sort :: TestSort           -- ^ The sort of the test.
      , ft_path :: TestPath           -- ^ Hierarchival path.
      , ft_location :: Maybe Location -- ^ Place of definition.
      , ft_payload :: a               -- ^ A generic payload.
      }

-- | Flattened representation of tests.
type FlatTest = GenFlatTest Assertion

-- | A filter is a predicate on 'FlatTest'. If the predicate is 'True', the flat test is run.
type TestFilter = FlatTest -> Bool

-- | The summary result of a test.
data TestResult = Pass | Pending | Fail | Error
                deriving (Show, Read, Eq)

-- | A type synonym for time in milliseconds.
type Milliseconds = Int

-- | The result of a test run.
data RunResult
    = RunResult
      { rr_result :: TestResult       -- ^ The summary result of the test.
      , rr_location :: Maybe Location -- ^ The location where the test failed (if applicable).
      , rr_message :: String          -- ^ A message describing the result.
      , rr_wallTimeMs :: Milliseconds -- ^ Execution time in milliseconds.
      }

-- | The result of running a 'FlatTest'
type FlatTestResult = GenFlatTest RunResult

-- | The state type for the 'TR' monad.
data TestState = TestState { ts_results :: [FlatTestResult] -- ^ Results collected so far.
                           , ts_index :: Int                -- ^ Current index for splitted output.
                           }

-- | The initial test state.
initTestState :: TestState
initTestState = TestState [] 0

-- | The 'TR' monad.
type TR = RWST TestConfig () TestState IO

-- | The destination of progress and result messages from HTF.
data TestOutput = TestOutputHandle Handle Bool -- ^ Output goes to 'Handle', boolean flag indicates whether the handle should be closed at the end.
                | TestOutputSplitted FilePath  -- ^ Output goes to files whose names are derived from 'FilePath' by appending a number to it. Numbering starts at zero.
                  deriving (Show, Eq)

-- | Configuration of test execution.
data TestConfig
    = TestConfig
      { tc_quiet :: Bool                -- ^ If set, displays messages only for failed tests.
      , tc_threads :: Maybe Int         -- ^ Use @Just i@ for parallel execution with @i@ threads, @Nothing@ for sequential execution (currently unused).
      , tc_output :: TestOutput         -- ^ Output destination of progress and result messages.
      , tc_filter :: TestFilter         -- ^ Filter for the tests to run.
      , tc_reporters :: [TestReporter]  -- ^ Test reporters to use.
      }

instance Show TestConfig where
    showsPrec prec tc =
        showParen (prec > 0) $
        showString "TestConfig { " .
        showString "tc_quiet=" . showsPrec 1 (tc_quiet tc) .
        showString ", tc_threads=" . showsPrec 1 (tc_threads tc) .
        showString ", tc_output=" . showsPrec 1 (tc_output tc) .
        showString ", tc_filter=<filter>" .
        showString ", tc_reporters=" . showsPrec 1 (tc_reporters tc) .
        showString " }"

-- | A 'TestReporter' provides hooks to customize the output of HTF.
data TestReporter
    = TestReporter
      { tr_id :: String
      , tr_reportAllTests :: ReportAllTests        -- ^ Called to report the IDs of all tests available.
      , tr_reportGlobalStart :: ReportGlobalStart  -- ^ Called to report the start of test execution.
      , tr_reportTestStart :: ReportTestStart      -- ^ Called to report the start of a single test.
      , tr_reportTestResult :: ReportTestResult    -- ^ Called to report the result of a single test.
      , tr_reportGlobalResults :: ReportGlobalResults  -- ^ Called to report the overall results of all tests.
      }

instance Show TestReporter where
    showsPrec _ x = showString (tr_id x)

instance Eq TestReporter where
    x == y = (tr_id x) == (tr_id y)

-- | Reports the IDs of all tests available.
type ReportAllTests = [FlatTest] -> TR ()

-- | Signals that test execution is about to start.
type ReportGlobalStart = [FlatTest] -> TR ()

-- | Reports the start of a single test.
type ReportTestStart = FlatTest -> TR ()

-- | Reports the result of a single test.
type ReportTestResult = FlatTestResult -> TR ()

-- | Reports the overall results of all tests.
type ReportGlobalResults = Milliseconds     -- ^ wall time in ms
                        -> [FlatTestResult] -- ^ passed tests
                        -> [FlatTestResult] -- ^ pending tests
                        -> [FlatTestResult] -- ^ failed tests
                        -> [FlatTestResult] -- ^ erroneous tests
                        -> TR ()
