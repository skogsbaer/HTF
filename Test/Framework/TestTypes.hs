{-# LANGUAGE FlexibleInstances #-}
{-|

This module defines types (and small auxiliary functions)
for organizing tests, for configuring the execution of
tests, and for representing and reporting their results.

-}
module Test.Framework.TestTypes (

  -- * Organizing tests
  TestID, Test(..), TestOptions(..), AssertionWithTestOptions(..), WithTestOptions(..),
  TestSuite(..), TestSort(..),
  TestPath(..), GenFlatTest(..), FlatTest, TestFilter,
  testPathToList, flatName, finalName, prefixName, defaultTestOptions, withOptions, historyKey,

  -- * Executing tests
  TR, TestState(..), initTestState, TestConfig(..), TestOutput(..),

  -- * Reporting results
  ReportAllTests, ReportGlobalStart, ReportTestStart, ReportTestResult, ReportGlobalResults, ReportGlobalResultsArg(..),
  TestReporter(..), emptyTestReporter, attachCallStack, CallStack,

  -- * Specifying results.
  TestResult(..), FlatTestResult, Milliseconds, RunResult(..)

) where

import Test.Framework.Location
import Test.Framework.Colors
import Test.Framework.History
import Test.Framework.TestInterface

import Control.Monad.RWS
import System.IO
import Data.Maybe
import qualified Data.List as List
import qualified Data.Text as T

-- | Type for naming tests.
type TestID = String

-- | Type for distinguishing different sorts of tests.
data TestSort = UnitTest | QuickCheckTest | BlackBoxTest
              deriving (Eq,Show,Read)

-- | General options for tests
data TestOptions = TestOptions {
      to_parallel :: Bool
    }
    deriving (Eq,Show,Read)

-- | The default 'TestOptions'
defaultTestOptions :: TestOptions
defaultTestOptions = TestOptions {
                       to_parallel = True
                     }

-- | Something with 'TestOptions'
data WithTestOptions a = WithTestOptions {
      wto_options :: TestOptions
    , wto_payload :: a
    }
    deriving (Eq,Show,Read)

-- | Shortcut for constructing a 'WithTestOptions' value.
withOptions :: (TestOptions -> TestOptions) -> a -> WithTestOptions a
withOptions f x = WithTestOptions (f defaultTestOptions) x

-- | A type class for an assertion with 'TestOptions'.
class AssertionWithTestOptions a where
    testOptions :: a -> TestOptions
    assertion :: a -> Assertion

instance AssertionWithTestOptions (IO a) where
    testOptions _ = defaultTestOptions
    assertion io = io >> return ()

instance AssertionWithTestOptions (WithTestOptions (IO a)) where
    testOptions (WithTestOptions opts _) = opts
    assertion (WithTestOptions _ io) = io >> return ()

-- | Abstract type for tests and their results.
data Test = BaseTest TestSort TestID (Maybe Location) TestOptions Assertion
          | CompoundTest TestSuite

-- | Abstract type for test suites and their results.
data TestSuite = TestSuite TestID [Test]
               | AnonTestSuite [Test]

-- | A type denoting the hierarchical name of a test.
data TestPath = TestPathBase TestID
              | TestPathCompound (Maybe TestID) TestPath
                deriving (Show)

-- | Splits a 'TestPath' into a list of test identifiers.
testPathToList :: TestPath -> [Maybe TestID]
testPathToList (TestPathBase i) = [Just i]
testPathToList (TestPathCompound mi p) =
    mi : testPathToList p

-- | Creates a string representation from a 'TestPath'.
flatName :: TestPath -> String
flatName p =
    flatNameFromList (testPathToList p)

flatNameFromList :: [Maybe TestID] -> String
flatNameFromList l =
    List.intercalate ":" (map (fromMaybe "") l)

-- | Returns the final name of a 'TestPath'
finalName :: TestPath -> String
finalName (TestPathBase i) = i
finalName (TestPathCompound _ p) = finalName p

-- | Returns the name of the prefix of a test path. The prefix is everything except the
--   last element.
prefixName :: TestPath -> String
prefixName path =
    let l = case reverse (testPathToList path) of
              [] -> []
              (_:xs) -> reverse xs
    in flatNameFromList l

-- | Generic type for flattened tests and their results.
data GenFlatTest a
    = FlatTest
      { ft_sort :: TestSort           -- ^ The sort of the test.
      , ft_path :: TestPath           -- ^ Hierarchival path.
      , ft_location :: Maybe Location -- ^ Place of definition.
      , ft_payload :: a               -- ^ A generic payload.
      }

-- | Key of a flat test for the history database.
historyKey :: GenFlatTest a -> T.Text
historyKey ft = T.pack (flatName (ft_path ft))

-- | Flattened representation of tests.
type FlatTest = GenFlatTest (WithTestOptions Assertion)

-- | A filter is a predicate on 'FlatTest'. If the predicate is 'True', the flat test is run.
type TestFilter = FlatTest -> Bool

-- | A type for call-stacks
type CallStack = [(Maybe String, Location)]

-- | The result of a test run.
data RunResult
    = RunResult
      { rr_result :: TestResult       -- ^ The summary result of the test.
      , rr_location :: Maybe Location -- ^ The location where the test failed (if applicable).
      , rr_callers :: CallStack       -- ^ Information about the callers of the location where the test failed
      , rr_message :: ColorString     -- ^ A message describing the result.
      , rr_wallTimeMs :: Milliseconds -- ^ Execution time in milliseconds.
      , rr_timeout :: Bool            -- ^ 'True' if the execution took too long
      }

attachCallStack :: ColorString -> CallStack -> ColorString
attachCallStack msg callStack =
    case reverse callStack of
      [] -> msg
      l -> ensureNewlineColorString msg +++
           noColor (unlines (map formatCallStackElem l))
    where
      formatCallStackElem (mMsg, loc) =
          "  called from " ++ showLoc loc ++ (case mMsg of
                                                Nothing -> ""
                                                Just s -> " (" ++ s ++ ")")

-- | The result of running a 'FlatTest'
type FlatTestResult = GenFlatTest RunResult

-- | The state type for the 'TR' monad.
data TestState = TestState { ts_results :: [FlatTestResult] -- ^ Results collected so far.
                           , ts_index :: Int                -- ^ Current index for splitted output.
                           }

-- | The initial test state.
initTestState :: TestState
initTestState = TestState [] 0

-- | The 'TR' (test runner) monad.
type TR = RWST TestConfig () TestState IO

-- | The destination of progress and result messages from HTF.
data TestOutput = TestOutputHandle Handle Bool -- ^ Output goes to 'Handle', boolean flag indicates whether the handle should be closed at the end.
                | TestOutputSplitted FilePath  -- ^ Output goes to files whose names are derived from 'FilePath' by appending a number to it. Numbering starts at zero.
                  deriving (Show, Eq)

-- | Configuration of test execution.
data TestConfig
    = TestConfig
      { tc_quiet :: Bool                -- ^ If set, displays messages only for failed tests.
      , tc_threads :: Maybe Int         -- ^ Use @Just i@ for parallel execution with @i@ threads, @Nothing@ for sequential execution.
      , tc_shuffle :: Bool              -- ^ Shuffle tests before parallel execution
      , tc_output :: TestOutput         -- ^ Output destination of progress and result messages.
      , tc_outputXml :: Maybe FilePath  -- ^ Output destination of XML result summary
      , tc_filter :: TestFilter         -- ^ Filter for the tests to run.
      , tc_reporters :: [TestReporter]  -- ^ Test reporters to use.
      , tc_useColors :: Bool            -- ^ Whether to use colored output
      , tc_historyFile :: FilePath      -- ^ Path to history file
      , tc_history :: TestHistory       -- ^ History of previous test runs
      , tc_sortByPrevTime :: Bool       -- ^ Sort ascending by previous execution times
      , tc_failFast :: Bool             -- ^ Stop test run as soon as one test fails
      , tc_timeoutIsSuccess :: Bool     -- ^ Do not regard timeout as an error
      , tc_maxSingleTestTime :: Maybe Milliseconds -- ^ Maximum time in milliseconds a single test is allowed to run
      , tc_prevFactor :: Maybe Double   -- ^ Maximum factor a single test is allowed to run slower than its previous execution
      , tc_repeat :: Int                -- ^ Number of times to repeat tests selected on the command line before reporting them as a success.
      }

instance Show TestConfig where
    showsPrec prec tc =
        showParen (prec > 0) $
        showString "TestConfig { " .
        showString "tc_quiet=" . showsPrec 1 (tc_quiet tc) .
        showString ", tc_threads=" . showsPrec 1 (tc_threads tc) .
        showString ", tc_shuffle=" . showsPrec 1 (tc_shuffle tc) .
        showString ", tc_output=" . showsPrec 1 (tc_output tc) .
        showString ", tc_outputXml=" . showsPrec 1 (tc_outputXml tc) .
        showString ", tc_filter=<filter>" .
        showString ", tc_reporters=" . showsPrec 1 (tc_reporters tc) .
        showString ", tc_useColors=" . showsPrec 1 (tc_useColors tc) .
        showString ", tc_historyFile=" . showsPrec 1 (tc_historyFile tc) .
        showString ", tc_history=" . showsPrec 1 (tc_history tc) .
        showString ", tc_sortByPrevTime=" . showsPrec 1 (tc_sortByPrevTime tc) .
        showString ", tc_failFast=" . showsPrec 1 (tc_failFast tc) .
        showString ", tc_timeoutIsSuccess=" . showsPrec 1 (tc_timeoutIsSuccess tc) .
        showString ", tc_maxSingleTestTime=" . showsPrec 1 (tc_maxSingleTestTime tc) .
        showString ", tc_prevFactor=" . showsPrec 1 (tc_prevFactor tc) .
        showString ", tc_repeat=" . showPrec 1 (tc_repeat tc) .
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

emptyTestReporter :: String -> TestReporter
emptyTestReporter id =
    TestReporter
      { tr_id = id
      , tr_reportAllTests = \_ -> return ()
      , tr_reportGlobalStart = \_ -> return ()
      , tr_reportTestStart = \_ -> return ()
      , tr_reportTestResult = \_ -> return ()
      , tr_reportGlobalResults = \_ -> return ()
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

data ReportGlobalResultsArg
    = ReportGlobalResultsArg
    { rgra_timeMs :: Milliseconds
    , rgra_passed :: [FlatTestResult]
    , rgra_pending :: [FlatTestResult]
    , rgra_failed :: [FlatTestResult]
    , rgra_errors :: [FlatTestResult]
    , rgra_timedOut :: [FlatTestResult]
    , rgra_filtered :: [FlatTest]
    }

-- | Reports the overall results of all tests.
type ReportGlobalResults = ReportGlobalResultsArg -> TR ()
