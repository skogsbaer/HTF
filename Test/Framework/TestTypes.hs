module Test.Framework.TestTypes where

import Test.Framework.Location

import Control.Monad.RWS
import System.IO

-- | An assertion is just an 'IO' action.
type Assertion = IO ()

-- | Type for naming tests.
type TestID = String

-- | Type for distinguishing different sorts of tests.
data TestSort = UnitTest | QuickCheckTest | BlackBoxTest
              deriving (Eq,Show,Read)

-- | Abstract type for tests.
data Test = BaseTest TestSort TestID (Maybe Location) Assertion
          | CompoundTest TestSuite

-- | Abstract type for test suites.
data TestSuite = TestSuite TestID [Test]
               | AnonTestSuite [Test]

-- | Type for flattened tests.
data FlatTest
    = FlatTest
      { ft_sort :: TestSort
      , ft_id :: TestID
      , ft_location :: Maybe Location
      , ft_assertion :: Assertion }

data TestResult = Pass | Pending | Fail | Error
                  deriving (Show, Read, Eq)

data TestState = TestState { ts_results  :: [RunResult] }

initTestState :: TestState
initTestState = TestState []

type TR = RWST TestConfig () TestState IO

data RunResult
    = RunResult
      { rr_sort :: TestSort
      , rr_id :: TestID
      , rr_location :: Maybe Location
      , rr_result :: TestResult }

-- | A filter is a predicate on 'FlatTest'. If the predicate is 'True', the flat test is run.
type TestFilter = FlatTest -> Bool

data TestConfig
    = TestConfig
      { tc_quiet :: Bool             -- ^ If set, displays messages only for failed tests
      , tc_threads :: Maybe Int      -- ^ Use @Just i@ for parallel execution with @i@ threads, @Nothing@ for sequential execution
      , tc_outputHandle :: Handle    -- ^ The output file
      , tc_filter :: TestFilter
      , tc_reporters :: [TestReporter]
      }

instance Show TestConfig where
    showsPrec prec tc =
        showParen (prec > 0) $
        showString "TestConfig { " .
        showString "tc_quiet=" . showsPrec 1 (tc_quiet tc) .
        showString ", tc_threads=" . showsPrec 1 (tc_threads tc) .
        showString ", tc_outputHandle=" . showsPrec 1 (tc_outputHandle tc) .
        showString ", tc_filter=<filter>" .
        showString ", tc_reporters" . showsPrec 1 (tc_reporters tc) .
        showString " }"

type ReportAllTests = [FlatTest] -> TR ()
type ReportGlobalStart = [FlatTest] -> TR ()
type ReportTestStart = FlatTest -> TR ()
type ReportTestResult = RunResult -> String -> TR ()
type ReportGlobalResults = [RunResult] -- ^ passed tests
                        -> [RunResult] -- ^ pending tests
                        -> [RunResult] -- ^ failed tests
                        -> [RunResult] -- ^ erroneous tests
                        -> TR ()

data TestReporter
    = TestReporter
      { tr_id :: String
      , tr_reportAllTests :: ReportAllTests
      , tr_reportGlobalStart :: ReportGlobalStart
      , tr_reportTestStart :: ReportTestStart
      , tr_reportTestResult :: ReportTestResult
      , tr_reportGlobalResults :: ReportGlobalResults
      }

instance Show TestReporter where
    showsPrec _ x = showString (tr_id x)

instance Eq TestReporter where
    x == y = (tr_id x) == (tr_id y)
