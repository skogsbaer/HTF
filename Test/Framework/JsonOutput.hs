{-# LANGUAGE OverloadedStrings #-}
module Test.Framework.JsonOutput (

    TestStartEventObj, TestEndEventObj, TestListObj, TestObj,

    mkTestStartEventObj, mkTestEndEventObj, mkTestListObj,

    decodeObj, HTFJsonObj

) where

import Test.Framework.TestTypes
import Test.Framework.Location

import qualified Data.Aeson as J
import Data.Aeson ((.=))

import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

{-

HTF's machine-readable output is a sequence of JSON messages. Each message is terminated
by a newline followed by two semicolons followed again by a newline.

There are three types of JSON messages. Each JSON object has a "type" attribute denoting
this type. The three types are "test-start", "test-end", and "test-list". Their haskell
representations are TestStartEventObj, TestEndEventObj, and TestListObj.
The corresponding JSON rendering is defined below.

-}

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
      , te_message :: String
      , te_timeInMs :: Int
      }

instance J.ToJSON TestEndEventObj where
    toJSON te =
        J.object ["type" .= J.String "test-end"
                 ,"test" .= J.toJSON (te_test te)
                 ,"result" .= J.toJSON (te_result te)
                 ,"message" .= J.toJSON (te_message te)
                 ,"time" .= J.toJSON (te_timeInMs te)]

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
                 ,"tests" .= J.Array (V.fromList (map J.toJSON (tlm_tests tl)))]

instance HTFJsonObj TestListObj

data TestObj
    = TestObj
      { to_id :: TestID
      , to_location :: Maybe Location
      }

instance J.ToJSON TestObj where
    toJSON t = J.object (["id" .= J.toJSON (to_id t)] ++
                         (case to_location t of
                            Just loc -> ["location" .= J.toJSON loc]
                            Nothing -> []))

instance J.ToJSON Location where
    toJSON loc = J.object ["file" .= J.toJSON (fileName loc)
                          ,"line" .= J.toJSON (lineNumber loc)]


mkTestStartEventObj :: TestID -> Maybe Location -> TestStartEventObj
mkTestStartEventObj id mLoc =
    TestStartEventObj (TestObj id mLoc)

mkTestEndEventObj :: TestID -> Maybe Location -> TestResult -> String -> Int -> TestEndEventObj
mkTestEndEventObj id mLoc r msg time =
    TestEndEventObj (TestObj id mLoc) r msg time

mkTestListObj :: [(TestID, Maybe Location)] -> TestListObj
mkTestListObj l =
    TestListObj (map (\(id, mLoc) -> TestObj id mLoc) l)

decodeObj :: HTFJsonObj a => a -> BSL.ByteString
decodeObj x =
    J.encode x `BSL.append` (BSLC.pack "\n;;\n")
