{- |

HTF's machine-readable output is a sequence of JSON messages. Each message is terminated
by a newline followed by two semicolons followed again by a newline.

There are four types of JSON messages. Each JSON object has a "type" attribute denoting
this type. The types are: @test-start@, @test-end@, and @test-list@, @test-results@.
Their haskell representations are 'TestStartEventObj', 'TestEndEventObj', 'TestListObj', and
'TestResultsObj'. The corresponding JSON rendering is defined in this module.

  *  The @test-start@ message denotes the start of a single test case. Example (whitespace inserted for better readability):

> {"test": {"flatName": "Main:nonEmpty",
>           "location": {"file": "Tutorial.hs", "line": 17},
>           "path": ["Main","nonEmpty"],
>           "sort": "unit-test"},
>  "type":"test-start"}

  *  The @test-end@ message denotes the end of a single test case. It contains information about the outcome of the test. Example:

> {"result": "pass",
>  "message":"",
>  "test":{"flatName": "Main:nonEmpty",
>          "location": {"file": "Tutorial.hs", "line": 17},
>          "path": ["Main","nonEmpty"],
>          "sort": "unit-test"},
>  "wallTime": 0,  // in milliseconds
>  "type": "test-end",
>  "location":null}

  *  The @test-results@ message occurs after all tests have been run and summarizes their results. Example:

> {"failures": 0,
>  "passed": 4,
>  "pending": 0,
>  "wallTime": 39, // in milliseconds
>  "errors": 0,
>  "type":"test-results"}

  *  The @test-list@ message contains all tests defined. It is used for the --list commandline options. Example:

> {"tests": [{"flatName":"Main:nonEmpty","location":{"file":"Tutorial.hs","line":17},"path":["Main","nonEmpty"],"sort":"unit-test"},
>            {"flatName":"Main:empty","location":{"file":"Tutorial.hs","line":19},"path":["Main","empty"],"sort":"unit-test"},
>            {"flatName":"Main:reverse","location":{"file":"Tutorial.hs","line":22},"path":["Main","reverse"],"sort":"quickcheck-property"},
>            {"flatName":"Main:reverseReplay","location":{"file":"Tutorial.hs","line":24},"path":["Main","reverseReplay"],"sort":"quickcheck-property"}],
>  "type":"test-list"}

For an exact specification, please have a look at the code of this module.
-}

{-# LANGUAGE OverloadedStrings #-}
module Test.Framework.JsonOutput (

    TestStartEventObj, TestEndEventObj, TestListObj, TestObj, TestResultsObj,

    mkTestStartEventObj, mkTestEndEventObj, mkTestListObj, mkTestResultsObj,

    decodeObj, HTFJsonObj

) where

import Test.Framework.TestTypes
import Test.Framework.Location

import qualified Data.Aeson as J
import Data.Aeson ((.=))

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

class J.ToJSON a => HTFJsonObj a

-- "test-start" message
data TestStartEventObj
    = TestStartEventObj
      { ts_test :: TestObj }

instance J.ToJSON TestStartEventObj where
    toJSON ts =
        J.object ["type" .= J.String "test-start"
                 ,"test" .= J.toJSON (ts_test ts)]

instance HTFJsonObj TestStartEventObj

-- "test-end" message
data TestEndEventObj
    = TestEndEventObj
      { te_test :: TestObj
      , te_result :: TestResult
      , te_location :: Maybe Location
      , te_callers :: [(Maybe String, Location)]
      , te_message :: String
      , te_wallTimeMs :: Int
      }

instance J.ToJSON TestEndEventObj where
    toJSON te =
        J.object ["type" .= J.String "test-end"
                 ,"test" .= J.toJSON (te_test te)
                 ,"location" .= J.toJSON (te_location te)
                 ,"callers" .= J.toJSON (map (\(msg, loc) -> J.object ["message" .= J.toJSON msg
                                                                      ,"location" .= J.toJSON loc])
                                             (te_callers te))
                 ,"result" .= J.toJSON (te_result te)
                 ,"message" .= J.toJSON (te_message te)
                 ,"wallTime" .= J.toJSON (te_wallTimeMs te)]

instance HTFJsonObj TestEndEventObj

instance J.ToJSON TestResult where
    toJSON r = J.String $
        case r of
          Pass -> "pass"
          Pending -> "pending"
          Fail -> "fail"
          Error -> "error"

-- "test-list" message
data TestListObj
    = TestListObj
      { tlm_tests :: [TestObj]
      }

instance J.ToJSON TestListObj where
    toJSON tl =
        J.object ["type" .= J.String "test-list"
                 ,"tests" .= J.toJSON (tlm_tests tl)]

instance HTFJsonObj TestListObj

-- "test-results"
data TestResultsObj
    = TestResultsObj
      { tr_wallTimeMs :: Int
      , tr_passed :: Int
      , tr_pending :: Int
      , tr_failed :: Int
      , tr_errors :: Int
      }

instance J.ToJSON TestResultsObj where
    toJSON r = J.object ["type" .= J.String "test-results"
                        ,"passed" .= J.toJSON (tr_passed r)
                        ,"pending" .= J.toJSON (tr_pending r)
                        ,"failures" .= J.toJSON (tr_failed r)
                        ,"errors" .= J.toJSON (tr_errors r)
                        ,"wallTime" .= J.toJSON (tr_wallTimeMs r)]

instance HTFJsonObj TestResultsObj

data TestObj
    = TestObj
      { to_flatName :: String
      , to_path :: TestPath
      , to_location :: Maybe Location
      , to_sort :: TestSort
      }

instance J.ToJSON TestObj where
    toJSON t = J.object (["flatName" .= J.toJSON (to_flatName t)
                         ,"path" .= J.toJSON (to_path t)
                         ,"sort" .= J.toJSON (to_sort t)] ++
                         (case to_location t of
                            Just loc -> ["location" .= J.toJSON loc]
                            Nothing -> []))

instance J.ToJSON TestPath where
    toJSON p = J.toJSON (testPathToList p)

instance J.ToJSON TestSort where
    toJSON s =
        case s of
          UnitTest -> J.String "unit-test"
          QuickCheckTest -> J.String "quickcheck-property"
          BlackBoxTest -> J.String "blackbox-test"


instance J.ToJSON Location where
    toJSON loc = J.object ["file" .= J.toJSON (fileName loc)
                          ,"line" .= J.toJSON (lineNumber loc)]


mkTestObj :: GenFlatTest a -> String -> TestObj
mkTestObj ft flatName =
    TestObj flatName (ft_path ft) (ft_location ft) (ft_sort ft)

mkTestStartEventObj :: FlatTest -> String -> TestStartEventObj
mkTestStartEventObj ft flatName =
    TestStartEventObj (mkTestObj ft flatName)

mkTestEndEventObj :: FlatTestResult -> String -> TestEndEventObj
mkTestEndEventObj ftr flatName =
    let r = ft_payload ftr
    in TestEndEventObj (mkTestObj ftr flatName) (rr_result r) (rr_location r) (rr_callers r)
                       (rr_message r) (rr_wallTimeMs r)

mkTestListObj :: [(FlatTest, String)] -> TestListObj
mkTestListObj l =
    TestListObj (map (\(ft, flatName) -> mkTestObj ft flatName) l)

mkTestResultsObj :: Milliseconds -> Int -> Int -> Int -> Int -> TestResultsObj
mkTestResultsObj time passed pending failed errors =
    TestResultsObj
    { tr_wallTimeMs = time
    , tr_passed = passed
    , tr_pending = pending
    , tr_failed = failed
    , tr_errors = errors
    }

decodeObj :: HTFJsonObj a => a -> BSL.ByteString
decodeObj x =
    J.encode x `BSL.append` (BSLC.pack "\n;;\n")
