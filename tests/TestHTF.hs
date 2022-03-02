{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -F -pgmF ./scripts/local-htfpp #-}
--
-- Copyright (c) 2005,2010   Stefan Wehr - http://www.stefanwehr.de
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--
import Test.Framework
import Test.Framework.Location
import Test.Framework.TestManager
import Test.Framework.BlackBoxTest
import Test.Framework.TestInterface

import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO
import System.IO.Temp
import Control.Exception
import Control.Monad

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KM
#else
import qualified Data.HashMap.Strict as KM
#endif
import qualified Data.HashSet as Set
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import Data.Aeson ( (.=) )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.List as List
import qualified Text.Regex as R
import GHC.Stack

import {-@ HTF_TESTS @-} qualified TestHTFHunitBackwardsCompatible
import {-@ HTF_TESTS @-} qualified Foo.A as A
import {-@ HTF_TESTS @-} Foo.B

import FailFast
import MaxCurTime
import MaxPrevTime
import UniqTests1
import UniqTests2
import PrevFactor
import Repeat
import SortByPrevTime
import Quasi

import Tutorial hiding (main)

data T = A | B
       deriving Eq
{-
stringGap = "hello \
            \world!"
-}
stringGap = "hello world!"

handleExc :: a -> SomeException -> a
handleExc x _ = x

-- 世界不是英文的 Test for #47

-- Test for #48
data HVect (ts :: [*]) where
  HNil :: HVect '[]
  HCons :: t -> HVect ts -> HVect (t ': ts)

fun :: HVect '[Int, Int] -> String
fun = undefined

-- Test for #45
foobar = [q|INSERT INTO a (name) VALUES ('')|]

test_assertFailure_FAIL = assertFailure "I'm a failure"

test_stringGap_OK = assertEqual stringGap "hello world!"

test_assertEqual_FAIL = assertEqual 1 2

test_assertEqualV_FAIL = assertEqualVerbose "blub" 1 2

test_assertEqualNoShow_FAIL = withOptions (\opts -> opts { to_parallel = False }) $
                              assertEqualNoShow A B

test_assertListsEqualAsSets_FAIL = assertListsEqualAsSets [1,2] [2]

test_assertSetEqualSuccess_OK = assertListsEqualAsSets [1,2] [2,1]

test_assertNotEmpty_FAIL = assertNotEmpty []

test_assertEmpty_FAIL = assertEmpty [1]

test_assertElem_FAIL = assertElem 1 [0,2,3]

test_assertThrows_FAIL = assertThrows (return () :: IO ()) (handleExc True)

test_assertThrows'_FAIL = assertThrows (error "ERROR") (handleExc False)

test_assertThrowsIO1_FAIL = assertThrows (fail "ERROR" :: IO ()) (handleExc False)

test_assertThrowsIO2_OK = assertThrowsIO (fail "ERROR") (handleExc True)

test_someError_ERROR = error "Bart Simpson!!" :: IO ()

test_pendingTest_PENDING = unitTestPending "This test is pending"

test_subAssert_FAIL = subAssert anotherSub
    where
      anotherSub = subAssertVerbose "I'm another sub" (assertNegative 42)
      assertNegative n = assertBool (n < 0)

data Expr = PlusExpr Expr Expr
          | MultExpr Expr Expr
          | Literal Int
          | Variable String
            deriving (Eq, Show)

test_diff_FAIL =
    assertEqual (mkExpr 1) (mkExpr 2)
    where
      mkExpr i =
          PlusExpr (PlusExpr (MultExpr (PlusExpr (Variable "foo")
                                                     (MultExpr (Literal 42) (Variable "bar")))
                                       (PlusExpr (Literal i) (Literal 2)))
                             (Literal 581))
                   (Variable "egg")

prop_ok_OK :: [Int] -> Property
prop_ok_OK xs = classify (null xs) "trivial" $ xs == (reverse (reverse xs))

prop_fail_FAIL :: [Int] -> Bool
prop_fail_FAIL xs = xs == (reverse xs)

prop_pendingProp_PENDING :: Int -> Bool
prop_pendingProp_PENDING x = qcPending "This property is pending" (x == 0)

prop_exhaust_FAIL = False ==> True

prop_error_FAIL :: Bool
prop_error_FAIL = error "Lisa"

changeArgs args = args { maxSuccess = 1 }

prop_ok'_OK = withQCArgs (\a -> a { maxSuccess = 1}) $
                        \xs -> classify (null xs) "trivial" $
                               (xs::[Int]) == (reverse (reverse xs))

prop_fail'_FAIL =
    prop
    where prop xs = xs == (reverse xs)
              where types = xs::[Int]

prop_error'_FAIL :: WithQCArgs Bool
prop_error'_FAIL = withQCArgs changeArgs $ (error "Lisa" :: Bool)

test_genericAssertions_OK =
    case test1 of
      AssertOk _ -> fail "did not expect AssertOk"
      AssertFailed stack msg ->
          do assertEqualVerbose ("stack=" ++ show stack) 2 (length (htfStackToList stack))
             let [se1, se2] = htfStackToList stack
                 loc1 = hse_location se1
                 loc2 = hse_location se2
             assertEqual (fileName loc1) (fileName loc2)
             assertEqual (lineNumber loc1) (line - 1)
             assertEqual (lineNumber loc2) (line - 3)
             assertNotEqual msg ""
    where
      test1 = test2
      test2 :: (HasCallStack) => AssertBool ()
      test2 = gassertBool False
      line = __LINE__

-- find . -name '*.hs' | xargs egrep -w -o -h "[a-zA-Z0-9_']+_PENDING" | sed 's/test_//g; s/prop_//g' | sort -u
pendingTests :: [T.Text]
pendingTests =
    ["pendingProp_PENDING"
    ,"pendingTest_PENDING"]

failedTests :: [T.Text]
failedTests =
-- $ find . -name '*.hs' | xargs egrep -w -o -h "[a-zA-Z0-9_']+_FAIL" | sed 's/test_//g; s/prop_//g' | sort -u
    ["1_FAIL"
    ,"a_FAIL"
    ,"h_FAIL"
    ,"assertElem_FAIL"
    ,"assertEmpty_FAIL"
    ,"assertEqualNoShow_FAIL"
    ,"assertEqualV_FAIL"
    ,"assertEqual_FAIL"
    ,"assertFailure_FAIL"
    ,"assertListsEqualAsSets_FAIL"
    ,"assertNotEmpty_FAIL"
    ,"assertThrows'_FAIL"
    ,"assertThrowsIO1_FAIL"
    ,"assertThrows_FAIL"
    ,"diff_FAIL"
    ,"error'_FAIL"
    ,"error_FAIL"
    ,"exhaust_FAIL"
    ,"fail'_FAIL"
    ,"fail_FAIL"
    ,"subAssert_FAIL"
-- $ find bbt -name '*not_ok*.x'
    ,"bbt/should_fail/not_ok_because_stderr1.x"
    ,"bbt/should_fail/not_ok_because_stderr2.x"
    ,"bbt/should_fail/not_ok_because_stdout1.x"
    ,"bbt/should_fail/not_ok_because_stdout2.x"
    ,"bbt/should_fail/not_ok_because_succeeds.x"
    ,"bbt/should_pass/not_ok_because_fails.x"
    ,"bbt/should_pass/not_ok_because_stderr1.x"
    ,"bbt/should_pass/not_ok_because_stderr2.x"
    ,"bbt/should_pass/not_ok_because_stdout1.x"
    ,"bbt/should_pass/not_ok_because_stdout2.x"
    ,"bbt/Verbose/not_ok_because_stdout1.x"]

-- $ find . -name '*.hs' | xargs egrep -w -o -h "[a-zA-Z0-9_']+_ERROR" | sed 's/test_//g; s/prop_//g' | sort -u
errorTests :: [T.Text]
errorTests = ["someError_ERROR"]

passedTests :: [T.Text]
passedTests =
    -- $ find . -name '*.hs' | xargs egrep -w -o -h "[a-zA-Z0-9_']+_OK" | sed 's/test_//g; s/prop_//g' | sort -u
    ["2_OK"
    ,"assertSetEqualSuccess_OK"
    ,"assertThrowsIO2_OK"
    ,"b_OK"
    ,"genericAssertions_OK"
    ,"ok'_OK"
    ,"ok_OK"
    ,"stringGap_OK"
-- $ find bbt -name '*ok*.x' | grep -v not_ok
    ,"bbt/should_fail/ok1.x"
    ,"bbt/should_fail/ok2.x"
    ,"bbt/should_pass/ok1.x"
    ,"bbt/should_pass/ok2.x"
    ,"bbt/should_pass/stdin_ok.x"]

timedOutTests = []

checkOutput output =
    do bsl <- BSL.readFile output
       let jsons = map (fromJust . J.decode) (splitJson bsl)
       let (pass, fail, error, pending, timedOut) = foldl checkStatus ([], [], [], [], []) jsons
       checkAsSet "passed" passedTests pass
       checkAsSet "failed" failedTests fail
       checkAsSet "errors" errorTests error
       checkAsSet "pending" pendingTests pending
       checkAsSet "timed-out" timedOutTests timedOut
       check jsons (J.object ["type" .= J.String "test-results"])
                   (J.object ["failures" .= J.toJSON (length failedTests)
                             ,"passed" .= J.toJSON (length passedTests)
                             ,"pending" .= J.toJSON (length pendingTests)
                             ,"errors" .= J.toJSON (length errorTests)
                             ,"timedOut" .= J.toJSON (length timedOutTests)])
       check jsons (J.object ["type" .= J.String "test-end"
                             ,"test" .= J.object ["flatName" .= J.String "Main:diff_FAIL"]])
                   (J.object ["test" .= J.object ["location" .= J.object ["file" .= J.String "TestHTF.hs"
                                                                         ,"line" .= J.toJSON (106+lineOffset)]]
                             ,"location" .= J.object ["file" .= J.String "TestHTF.hs"
                                                     ,"line" .= J.toJSON (107+lineOffset)]])
       check jsons (J.object ["type" .= J.String "test-end"
                             ,"test" .= J.object ["flatName" .= J.String "Foo.A:a_FAIL"]])
                   (J.object ["test" .= J.object ["location" .= J.object ["file" .= J.String "Foo/A.hs"
                                                                         ,"line" .= J.toJSON (10::Int)]]
                             ,"location" .= J.object ["file" .= J.String "./Foo/A.hs"
                                                     ,"line" .= J.toJSON (11::Int)]])
       check jsons (J.object ["type" .= J.String "test-end"
                             ,"test" .= J.object ["flatName" .= J.String "Foo.A:h_FAIL"]])
                   (J.object ["test" .= J.object ["location" .= J.object ["file" .= J.String "Foo/test.h"
                                                                         ,"line" .= J.toJSON (8::Int)]]
                             ,"location" .= J.object ["file" .= J.String "./Foo/test.h"
                                                     ,"line" .= J.toJSON (9::Int)]])
       check jsons (J.object ["type" .= J.String "test-end"
                             ,"test" .= J.object ["flatName" .= J.String "Main:subAssert_FAIL"]])
                   (J.object ["callers" .= J.toJSON [J.object ["message" .= J.String "I'm another sub"
                                                              ,"location" .= J.object ["file" .= J.String "TestHTF.hs"
                                                                                      ,"line" .= J.toJSON (97+lineOffset)]]
                                                    ,J.object ["message" .= J.Null
                                                              ,"location" .= J.object ["file" .= J.String "TestHTF.hs"
                                                                                      ,"line" .= J.toJSON (95+lineOffset)]]]])
    where
      lineOffset :: Int
      lineOffset = 41
      checkStatus tuple@(pass, fail, error, pending, timedOut) json =
          {-
            {"location":null
            ,"test":{"path":["Main","tests/bbt/should_pass/stdin_ok.x"],"sort":"blackbox-test","flatName":"Main:tests/bbt/should_pass/stdin_ok.x"}
            ,"callers":[]
            ,"result":"pass"
            ,"timedOut":false
            ,"type":"test-end"
            ,"message":""
            ,"wallTime":11}
           -}
          case json of
            J.Object objJson | Just (J.Object testObj) <- KM.lookup "test" objJson
                             , Just (J.String flatName) <- KM.lookup "flatName" testObj
                             , Just (J.String "test-end") <- KM.lookup "type" objJson
                             , Just (J.String result) <- KM.lookup "result" objJson
                             , Just (J.Bool to) <- KM.lookup "timedOut" objJson ->
                let shortName =
                        let t = T.tail (T.dropWhile (/= ':') flatName)
                        in if "tests/" `T.isPrefixOf` t
                           then T.drop (T.length "tests/") t
                           else t
                    newTimedOut = if to then shortName : timedOut else timedOut
                in case () of
                     _| result == "pass" -> (shortName : pass, fail, error, pending, newTimedOut)
                     _| result == "fail" -> (pass, shortName : fail, error, pending, newTimedOut)
                     _| result == "error" -> (pass, fail, shortName : error, pending, newTimedOut)
                     _| result == "pending" -> (pass, fail, error, shortName : pending, newTimedOut)
            _ -> tuple
      checkAsSet what expList givenList =
          let expSet = Set.fromList expList
              givenSet = Set.fromList givenList
          in if expSet == givenSet
             then return ()
             else do let unexpected = givenSet `Set.difference` expSet
                         notGiven = expSet `Set.difference` givenSet
                     fail ("Mismatch for " ++ what ++ ":" ++
                           "\nExpected: " ++ show expList ++
                           "\nGiven: " ++ show givenList ++
                           "\nUnexpected elements: " ++ show unexpected ++
                           "\nElements expected but not present: " ++ show notGiven)
      check jsons pred assert =
          case filter (\j -> matches j pred) jsons of
            [json] ->
                if not (matches json assert)
                   then error ("Predicate\n" ++ ppJ pred ++ " match JSON\n" ++ ppJ json ++ ", but assertion\n" ++
                               ppJ assert ++ " not satisfied")
                   else return ()
            l -> error ("not exactly one JSON matches predicate " ++ show pred ++ " but " ++ show l)
      matches :: J.Value -> J.Value -> Bool
      matches json pred =
          case (json, pred) of
            (J.Object objJson, J.Object objPred) ->
                KM.foldrWithKey (\k vPred b ->
                                    b && case KM.lookup k objJson of
                                           Just vJson -> matches vJson vPred
                                           Nothing -> False)
                               True objPred
            (J.String strJson, J.String strPred) ->
                regexMatches (mkRegex strPred) strJson
            (arrJson@(J.Array _), arrPred@(J.Array _)) ->
                let J.Success (listJson :: [J.Value]) = J.fromJSON arrJson
                    J.Success (listPred :: [J.Value]) = J.fromJSON arrPred
                in length listJson == length listPred &&
                   all (\(x, y) -> matches x y) (zip listJson listPred)
            _ -> json == pred
      regexMatches r s = isJust $ R.matchRegex r (T.unpack s)
      mkRegex s = R.mkRegexWithOpts (T.unpack s) True False
      splitJson bsl =
          if BSL.null bsl
             then []
             else case BSL.span (/= 10) bsl of
                    (start, rest) ->
                        if BSLC.pack "\n;;\n" `BSL.isPrefixOf` rest
                           then start : splitJson (BSL.drop 4 rest)
                           else case splitJson rest of
                                  [] -> error "invalid json output from HTF"
                                  (x:xs) -> (start `BSL.append` x : xs)
      ppJ json =
          T.unpack $
          T.decodeUtf8With T.lenientDecode $
          BSL.toStrict $
          J.encodePretty' J.defConfig json

runRealBlackBoxTests =
    do b <- doesDirectoryExist "tests/bbt"
       let dirPrefix = if b then "tests" else ""
       bbts <- blackBoxTests (dirPrefix </> "real-bbt") ("/bin/bash") ".sh"
                 (defaultBBTArgs { bbtArgs_verbose = True })
       ecode <- runTest bbts
       case ecode of
         ExitFailure _ -> fail ("real blackbox tests failed!")
         _ -> return ()

main =
    do args <- getArgs
       b <- doesDirectoryExist "tests/bbt"
       let dirPrefix = if b then "tests" else ""
       bbts <- blackBoxTests (dirPrefix </> "bbt") (dirPrefix </> "./run-bbt.sh") ".x"
                 (defaultBBTArgs { bbtArgs_verbose = False })
       let tests = [addToTestSuite htf_thisModulesTests bbts] ++ htf_importedTests
       when ("--help" `elem` args || "-h" `elem` args) $
            do hPutStrLn stderr ("USAGE: PROG [--direct]")
               ecode <- runTestWithArgs ["--help"] ([] :: [Test])
               exitWith ecode
       case args of
         "FailFast.hs":rest -> failFastMain rest
         "MaxCurTime.hs":rest -> maxCurTimeMain rest
         "MaxPrevTime.hs":rest -> maxPrevTimeMain rest
         "PrevFactor.hs":rest -> prevFactorMain rest
         "Repeat.hs":rest -> repeatMain rest
         "SortByPrevTime.hs":rest -> sortByPrevTimeMain rest
         "UniqTests1.hs":rest -> uniqTests1Main rest
         "UniqTests2.hs":rest -> uniqTests2Main rest
         x:_ | ".hs" `List.isSuffixOf` x -> fail ("Unkown real-bbt test: " ++ x)
         "--direct":rest ->
             do ecode <- runTestWithArgs rest tests
                case ecode of
                  ExitFailure _ -> return ()
                  _ -> fail ("unexpected exit code: " ++ show ecode)
         _ ->
             do withSystemTempFile "HTF-out" $ \outFile h ->
                  do hClose h
                     ecode <- runTestWithArgs ["-j4", "--shuffle=false",
                                               "--json", "--output-file=" ++ outFile] tests
                     case ecode of
                       ExitFailure _ -> checkOutput outFile
                       _ -> fail ("unexpected exit code: " ++ show ecode)
                     `onException` (do s <- readFile outFile
                                       hPutStrLn stderr s)
                runRealBlackBoxTests
