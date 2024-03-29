{-# LANGUAGE CPP #-}
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

XML-output following the JUnit output format.

The data types exposed by this module give a rough specification of
the output format.

Here is a sample ouput:

@
\<?xml version="1.0" encoding="UTF-8" standalone="yes"?\>
\<testsuites tests="6" failures="2" errors="0" time="0.705"\>
  \<testsuite id="0" tests="2" failures="0" errors="0" time="0.000" name="MyPkg.A" package="MyPkg.A"\>
    \<testcase classname="MyPkg.A" name="test_funA2" time="0.000"/\>
    \<testcase classname="MyPkg.A" name="test_funA1" time="0.000"/\>
  \</testsuite\>
  \<testsuite id="1" tests="2" failures="0" errors="0" time="0.000" name="MyPkg.B" package="MyPkg.B"\>
    \<testcase classname="MyPkg.B" name="test_funB2" time="0.000"/\>
    \<testcase classname="MyPkg.B" name="test_funB1" time="0.000"/\>
  \</testsuite\>
  \<testsuite id="2" tests="2" failures="2" errors="0" time="0.703" name="bbts" package="bbts"\>
    \<testcase classname="bbts" name="bbt_bbt-dir/should-pass/x.num" time="0.230"\>
      \<failure type="failure" message="test is supposed to succeed but failed with exit code 255"\>test is supposed to succeed but failed with exit code 255\</failure\>
    \</testcase\>
    \<testcase classname="bbts" name="bbt_bbt-dir/should-fail/z.num" time="0.473"\>
      \<failure type="failure" message="Mismatch on stderr:"\>Mismatch on stderr:
--- bbt-dir/should-fail/z.err	2015-09-05 18:37:30.000000000 +0200
+++ -	2022-03-06 09:49:55.480265000 +0100
\@\@ -1 +1 \@\@
-invalid input
+sample[88331]: [fatal] unable to read input graph: The data couldn’t be read because it isn’t in the correct format.
[end of diff output]
\</failure\>
    \</testcase\>
  \</testsuite\>
\</testsuites\>
@

-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Framework.XmlOutput (

  JunitXmlOutput(..), Testsuites(..), Testsuite(..), Testcase(..), Result(..)
  , mkGlobalResultsXml

) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List

#ifndef MIN_VERSION_containers
#define MIN_VERSION_containers(a,b,c) 1
#endif
#if MIN_VERSION_containers(0,5,0)
import qualified Data.Map.Strict as Map
#else
import qualified Data.Map as Map
#endif

import qualified Data.Text as T
import Text.Printf

import Text.XML.Generator

import Test.Framework.TestTypes
import Test.Framework.Colors

-- | A "specification" of the output format in terms of haskell data types:
-- The name of each data type corresponds to the name of an XML element
-- (lowercase first letter).
-- The name of a field with a primitive corresponds to an attribute with
-- then same name as the field (without the prefix up to the first @_@).
--
-- The root element is @testsuites@
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
      msg = renderColorString (attachCallStack (rr_message payload) (rr_stack payload)) False

millisToSeconds :: Milliseconds -> Seconds
millisToSeconds millis =
    fromInteger (toInteger millis) / 1000.0

mkGlobalResultsXml :: ReportGlobalResultsArg -> BSL.ByteString
mkGlobalResultsXml arg =
    let nPassed = length (rgra_passed arg)
        nPending = length (rgra_pending arg)
        nFailed = length (rgra_failed arg)
        nErrors = length (rgra_errors arg)
        byModules = groupByModule (rgra_passed arg ++ rgra_pending arg ++
                                   rgra_failed arg ++ rgra_errors arg)
        suites = map mkTestSuite (zip [0..] byModules)
        root = Testsuites
               { tss_tests = nPassed + nPending + nFailed + nErrors
               , tss_failures = nFailed
               , tss_errors = nErrors
               , tss_time = millisToSeconds (rgra_timeMs arg)
               , tss_suites = suites }
    in renderAsXml (JunitXmlOutput root)
