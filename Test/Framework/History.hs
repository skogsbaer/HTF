{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{- |
Internal module for retaining a history of test runs.
-}
module Test.Framework.History (

    TestHistory, HistoricTestResult(..), emptyTestHistory, Milliseconds, TestResult(..)
  , serializeTestHistory, deserializeTestHistory
  , findHistoricTestResult, findHistoricSuccessfulTestResult
  , updateTestHistory, mkTestRunHistory
  , historyTests

) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import qualified Data.Vector as V
import Data.Time.Clock
import Test.HUnit
import Data.Aeson hiding (Error)
import Data.Aeson.TH
import Test.Framework.TestInterface

-- | A type synonym for time in milliseconds.
type Milliseconds = Int

data TestHistory
    = TestHistory
      { th_runs :: !(V.Vector (TestRunHistory)) -- reverse chronologically sorted
      , th_index :: !(Map T.Text (HistoricTestResult))
      , th_successfulIndex :: !(Map T.Text (HistoricTestResult))
      }
    deriving (Eq)

instance Show (TestHistory) where
    showsPrec _ _ = showString "<TestHistory>"

emptyTestHistory :: TestHistory
emptyTestHistory =
    TestHistory V.empty Map.empty Map.empty

data TestRunHistory
    = TestRunHistory
      { trh_startTime :: !UTCTime
      , trh_tests :: !(V.Vector (HistoricTestResult))
      }
    deriving (Eq)

instance Show TestRunHistory where
    showsPrec d trh =
        showParen (d > 10) $
        showString "TestRunHistory <hidden time> " .
        showsPrec 11 (trh_tests trh)

data HistoricTestResult
    = HistoricTestResult
      { htr_testId :: !T.Text
      , htr_result :: !TestResult
      , htr_timedOut :: !Bool
      , htr_timeMs :: !Milliseconds
      }
    deriving (Show, Eq)

mkTestRunHistory :: UTCTime -> [HistoricTestResult] -> TestRunHistory
mkTestRunHistory time results = TestRunHistory {
                                  trh_startTime = time
                                , trh_tests = V.fromList results
                                }

isSuccess :: HistoricTestResult -> Bool
isSuccess r = htr_result r == Pass && not (htr_timedOut r)

updateTestHistory :: TestRunHistory -> TestHistory -> TestHistory
updateTestHistory runHistory history =
    let runs = runHistory : V.toList (th_runs history)
    in TestHistory (V.fromList runs) (createIndex runs (const True)) (createIndex runs isSuccess)

-- The [TestRunHistory] list is sorted reverse chronologically
createIndex :: [TestRunHistory] -> (HistoricTestResult -> Bool) -> Map T.Text (HistoricTestResult)
createIndex list pred =
    L.foldl' updateMap Map.empty flatRunHistory
    where
      updateMap m res =
          Map.insertWith (\_new old -> old) (htr_testId res) res m
      flatRunHistory =
          filter pred $ concatMap (\trh -> V.toList (trh_tests trh)) list

findHistoricTestResult :: T.Text -> TestHistory -> Maybe (HistoricTestResult)
findHistoricTestResult id hist = Map.lookup id (th_index hist)

findHistoricSuccessfulTestResult :: T.Text -> TestHistory -> Maybe (HistoricTestResult)
findHistoricSuccessfulTestResult id hist = Map.lookup id (th_successfulIndex hist)

data SerializableTestHistory
    = SerializableTestHistory
      { sth_version :: Int
      , sth_runs :: !(V.Vector (TestRunHistory)) -- reverse chronologically sorted
      }

_CURRENT_VERSION_ :: Int
_CURRENT_VERSION_ = 0

instance ToJSON TestResult where
    toJSON r = String $
        case L.lookup r testResultStringMapping of
          Just s -> s
          Nothing -> error ("TestResult " ++ show r ++ " not defined in testResultStringMapping")

instance FromJSON TestResult where
    parseJSON v =
        case v of
          String s
              | Just r <- L.lookup s (map (\(x, y) -> (y, x)) testResultStringMapping)
                       -> return r
          _ -> fail ("could not parse JSON value as a test result: " ++ show v)

testResultStringMapping :: [(TestResult, T.Text)]
testResultStringMapping =
    [(Pass, "pass"), (Pending, "pending"), (Fail, "fail"), (Error, "error")]

deriveJSON (defaultOptions { fieldLabelModifier = drop 4 }) ''HistoricTestResult
deriveJSON (defaultOptions { fieldLabelModifier = drop 4 }) ''TestRunHistory
deriveJSON (defaultOptions { fieldLabelModifier = drop 4 }) ''SerializableTestHistory

serializeTestHistory :: TestHistory -> BS.ByteString
serializeTestHistory hist =
    let serHist = SerializableTestHistory {
                    sth_version = _CURRENT_VERSION_
                  , sth_runs = th_runs hist
                  }
    in BSL.toStrict $ encode serHist

deserializeTestHistory :: BS.ByteString -> Either String (TestHistory)
deserializeTestHistory bs =
    -- assume version 0 for now. Later we have to look into the json, find the version and then decide which parser to user
    case decodeStrict bs of
      Nothing -> Left ("could not decode JSON: " ++ show bs)
      Just !serHist ->
          let list = V.toList (sth_runs serHist)
          in Right (TestHistory (sth_runs serHist) (createIndex list (const True)) (createIndex list isSuccess))

testCreateIndex =
    do time <- getCurrentTime
       let index = createIndex (historyList time) (const True)
       if index == expectedIndex
       then return ()
       else assertFailure ("== Expected index:\n" ++ show expectedIndex ++
                           "\n== Given index:\n" ++ show index)
    where
      historyList time =
          [mkHist time [mkRes "foo" 1]
          ,mkHist time [mkRes "foo" 2, mkRes "bar" 10]
          ,mkHist time [mkRes "bar" 20, mkRes "egg" 3]]
      expectedIndex = Map.fromList [("foo", mkRes "foo" 1)
                                   ,("bar", mkRes "bar" 10)
                                   ,("egg", mkRes "egg" 3)]
      mkHist time l = TestRunHistory time (V.fromList l)
      mkRes id ms = HistoricTestResult id Pass False ms

historyTests = [("testCreateIndex", testCreateIndex)]
