module Test.Framework.TestTypes where

import Control.Monad.RWS

import Test.Framework.Location
import Test.Framework.TestConfig

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

type TR = RWST TestConfig () TestState IO

data RunResult
    = RunResult
      { rr_sort :: TestSort
      , rr_id :: TestID
      , rr_location :: Maybe Location
      , rr_result :: TestResult }
