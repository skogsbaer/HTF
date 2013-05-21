{- |

See <http://pzolee.blogs.balabit.com/2012/11/jenkins-vs-junit-xml-format/>
for a description of the format used.

The source code of this module also contains a rough specification of
the output format in terms of Haskell data types.

-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Framework.XmlOutput (

    mkGlobalResultsXml

) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Text.Printf

import Text.XML.Generator

import Test.Framework.TestTypes
import Test.Framework.Colors

-- A "specification" of the output format in terms of haskell data types
-- * The name of each data type corresponds to the name of an XML element
--   (lowercase first letter)
-- * The name of a field with a primitive corresponds to an attribute with
--   then same name as the field (without the prefix)
data JunitXmlOutput = JunitXmlOutput Testsuites

type Seconds = Double

data Testsuites
    = Testsuites
      { tss_tests :: Int
      , tss_failures :: Int
      , tss_errors :: Int
      , tss_time :: Seconds
      , tss_suites :: [Testsuite] }

data Testsuite
    = Testsuite
      { ts_tests :: Int
      , ts_failures :: Int
      , ts_errors :: Int
      , ts_time :: Seconds
      , ts_id :: Int
      , ts_name :: String
      , ts_package :: String
      , ts_testcases :: [Testcase] }

data Testcase
    = Testcase
      { tc_classname :: String
      , tc_name :: String
      , tc_time :: Seconds
      , tc_result :: Maybe Result }

-- For this datatype, the elemName field specifies the name of the element
data Result
    = Result
      { r_elemName :: String
      , r_message :: T.Text
      , r_type :: String
      , r_textContent :: T.Text }

renderAsXml :: JunitXmlOutput -> BSL.ByteString
renderAsXml (JunitXmlOutput suites) =
    xrender $
    doc defaultDocInfo $
        xelem "testsuites" $
              xattr "tests" (showT (tss_tests suites)) <>
              xattr "failures" (showT (tss_failures suites)) <>
              xattr "errors" (showT (tss_errors suites)) <>
              xattr "time" (showTime (tss_time suites)) <#>
              (map testsuiteXml (tss_suites suites))
    where
      testsuiteXml suite =
          xelem "testsuite" $
                xattr "id" (showT (ts_id suite)) <>
                xattr "tests" (showT (ts_tests suite)) <>
                xattr "failures" (showT (ts_failures suite)) <>
                xattr "errors" (showT (ts_errors suite)) <>
                xattr "time" (showTime (ts_time suite)) <>
                xattr "name" (T.pack (ts_name suite)) <>
                xattr "package" (T.pack (ts_package suite)) <#>
                (map testcaseXml (ts_testcases suite))
      testcaseXml tc =
          xelem "testcase" $
                xattr "classname" (T.pack (tc_classname tc)) <>
                xattr "name" (T.pack (tc_name tc)) <>
                xattr "time" (showTime (tc_time tc)) <#>
                resultXml (tc_result tc)
      resultXml Nothing = xempty
      resultXml (Just res) =
          xelem (T.pack (r_elemName res)) $
                xattr "type" (T.pack (r_type res)) <>
                xattr "message" (r_message res) <#>
                xtext (r_textContent res)
      showT = T.pack . show
      showTime = T.pack . printf "%.3f"

groupByModule :: [FlatTestResult] -> [(String, [FlatTestResult])]
groupByModule l =
    let m = List.foldl' (\m r -> Map.insertWith (++) (prefixName (ft_path r)) [r] m) Map.empty l
    in Map.toList m

mkTestSuite :: (Int, (String, [FlatTestResult])) -> Testsuite
mkTestSuite (id, (modName, results)) =
    Testsuite
    { ts_tests = nTests
    , ts_failures = nFailures
    , ts_errors = nErrors
    , ts_time = millisToSeconds millis
    , ts_id = id
    , ts_name = modName
    , ts_package = modName
    , ts_testcases = map mkTestCase results }
    where
      (nTests, nFailures, nErrors, millis) =
          List.foldl' (\(t, f, e, m) r -> (t + 1, f + failureInc r, e + errorInc r,
                                           m + (rr_wallTimeMs . ft_payload) r))
                      (0, 0, 0, 0) results
      failureInc r = if isFailure r then 1 else 0
      errorInc r = if isError r then 1 else 0

isFailure :: FlatTestResult -> Bool
isFailure r = Fail == (rr_result . ft_payload) r

isError :: FlatTestResult -> Bool
isError r = Error == (rr_result . ft_payload) r

mkTestCase :: FlatTestResult -> Testcase
mkTestCase r =
    Testcase
    { tc_classname = modName
    , tc_name = simpleName
    , tc_time = millisToSeconds (rr_wallTimeMs payload)
    , tc_result = result }
    where
      payload = ft_payload r
      simpleName = prefix ++ finalName (ft_path r)
      modName = prefixName (ft_path r)
      prefix = case ft_sort r of
                 UnitTest -> "test_"
                 QuickCheckTest -> "prop_"
                 BlackBoxTest -> "bbt_"
      result =
          if isFailure r
          then Just (mkResult "failure")
          else if isError r
               then Just (mkResult "error")
               else Nothing
      mkResult elemName =
          Result
          { r_elemName = elemName
          , r_message = T.takeWhile (/= '\n') msg
          , r_type = elemName
          , r_textContent = msg }
      msg = renderColorString (attachCallStack (rr_message payload) (rr_callers payload)) False

millisToSeconds :: Milliseconds -> Seconds
millisToSeconds millis =
    fromInteger (toInteger millis) / 1000.0

mkGlobalResultsXml :: Milliseconds     -- ^ wall time in ms
                   -> [FlatTestResult] -- ^ passed tests
                   -> [FlatTestResult] -- ^ pending tests
                   -> [FlatTestResult] -- ^ failed tests
                   -> [FlatTestResult] -- ^ erroneous tests
                   -> BSL.ByteString
mkGlobalResultsXml t pass pending failed errors =
    let nPassed = length pass
        nPending = length pending
        nFailed = length failed
        nErrors = length errors
        byModules = groupByModule (pass ++ pending ++ failed ++ errors)
        suites = map mkTestSuite (zip [0..] byModules)
        root = Testsuites
               { tss_tests = nPassed + nPending + nFailed + nErrors
               , tss_failures = nFailed
               , tss_errors = nErrors
               , tss_time = millisToSeconds t
               , tss_suites = suites }
    in renderAsXml (JunitXmlOutput root)
