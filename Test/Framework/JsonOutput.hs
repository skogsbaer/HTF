{-# LANGUAGE OverloadedStrings #-}
--
-- Copyright (c) 2005-2022   Stefan Wehr - http://www.stefanwehr.de
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
module Test.Framework.JsonOutput (

    TestStartEventObj, TestEndEventObj, TestListObj, TestObj, TestResultsObj,

    mkTestStartEventObj, mkTestEndEventObj, mkTestListObj, mkTestResultsObj,

    decodeObj, HTFJsonObj

) where

import Test.Framework.TestTypes
import Test.Framework.Location
import Test.Framework.Colors
import Test.Framework.TestInterface

import qualified Data.Aeson as J
import Data.Aeson ((.=))

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T

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
      , te_stack :: HtfStack
      , te_message :: T.Text
      , te_wallTimeMs :: Int
      , te_timedOut :: Bool
      }

instance J.ToJSON TestEndEventObj where
    toJSON te =
        J.object ["type" .= J.String "test-end"
                 ,"test" .= J.toJSON (te_test te)
                 ,"location" .= J.toJSON (failureLocationFromStack (te_stack te))
                 ,"callers" .=
                    J.toJSON (map (\entry -> J.object ["location" .= J.toJSON (hse_location entry)
                                                      ,"message" .= J.toJSON (hse_message entry)])
                              (restCallStack (te_stack te)))
                 ,"result" .= J.toJSON (te_result te)
                 ,"message" .= J.toJSON (te_message te)
                 ,"wallTime" .= J.toJSON (te_wallTimeMs te)
                 ,"timedOut" .= J.toJSON (te_timedOut te)]

instance HTFJsonObj TestEndEventObj

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
      , tr_timedOut :: Int
      , tr_filtered :: Int
      }

instance J.ToJSON TestResultsObj where
    toJSON r = J.object ["type" .= J.String "test-results"
                        ,"passed" .= J.toJSON (tr_passed r)
                        ,"pending" .= J.toJSON (tr_pending r)
                        ,"failures" .= J.toJSON (tr_failed r)
                        ,"errors" .= J.toJSON (tr_errors r)
                        ,"timedOut" .= J.toJSON (tr_timedOut r)
                        ,"filtered" .= J.toJSON (tr_filtered r)
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
        msg = renderColorString (rr_message r) False
    in TestEndEventObj (mkTestObj ftr flatName) (rr_result r) (rr_stack r)
                       msg (rr_wallTimeMs r) (rr_timeout r)

mkTestListObj :: [(FlatTest, String)] -> TestListObj
mkTestListObj l =
    TestListObj (map (\(ft, flatName) -> mkTestObj ft flatName) l)

mkTestResultsObj :: ReportGlobalResultsArg -> TestResultsObj
mkTestResultsObj arg =
    TestResultsObj
    { tr_wallTimeMs = rgra_timeMs arg
    , tr_passed = length (rgra_passed arg)
    , tr_pending = length (rgra_pending arg)
    , tr_failed = length (rgra_failed arg)
    , tr_errors = length (rgra_errors arg)
    , tr_timedOut = length (rgra_timedOut arg)
    , tr_filtered = length (rgra_filtered arg)
    }

decodeObj :: HTFJsonObj a => a -> BSL.ByteString
decodeObj x =
    J.encode x `BSL.append` (BSLC.pack "\n;;\n")
