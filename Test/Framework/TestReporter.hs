{-# LANGUAGE OverloadedStrings #-}
{-|

This module defines functions for notifying all test reporters registered about
particular events in the lifecycle of a test run.

Further, it defines the standard test reporters for HTF's various output formats.

-}
module Test.Framework.TestReporter (

    IsParallel(..), isParallelFromBool, IsJsonOutput(..), IsXmlOutput(..),
    reportAllTests, reportGlobalStart, reportTestStart, reportTestResult,
    reportGlobalResults, defaultTestReporters

) where

import Test.Framework.TestTypes
import Test.Framework.Location
import Test.Framework.Colors
import Test.Framework.JsonOutput
import Test.Framework.XmlOutput

import System.IO
import Control.Monad.RWS
import Text.PrettyPrint

import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BSL

-- | Invokes 'tr_reportAllTests' on all test reporters registered.
reportAllTests :: ReportAllTests
reportAllTests tests =
    do reps <- asks tc_reporters
       mapM_ (\r -> tr_reportAllTests r tests) reps

-- | Invokes 'tr_reportGlobalStart' on all test reporters registered.
reportGlobalStart :: ReportGlobalStart
reportGlobalStart tests =
    do reps <- asks tc_reporters
       mapM_ (\r -> tr_reportGlobalStart r tests) reps

-- | Invokes 'tr_reportTestStart' on all test reporters registered.
reportTestStart :: ReportTestStart
reportTestStart t =
    do reps <- asks tc_reporters
       mapM_ (\r -> tr_reportTestStart r t) reps

-- | Invokes 'tr_reportTestResult' on all test reporters registered.
reportTestResult :: ReportTestResult
reportTestResult t =
    do reps <- asks tc_reporters
       mapM_ (\r -> tr_reportTestResult r t) reps

-- | Invokes 'tr_reportGlobalResults' on all test reporters registered.
reportGlobalResults :: ReportGlobalResults
reportGlobalResults arg =
    do reps <- asks tc_reporters
       mapM_ (\r -> tr_reportGlobalResults r arg) reps

data IsParallel = Parallel | NonParallel

isParallelFromBool :: Bool -> IsParallel
isParallelFromBool True = Parallel
isParallelFromBool False = NonParallel

data IsJsonOutput = JsonOutput | NoJsonOutput
data IsXmlOutput = XmlOutput | NoXmlOutput

-- | The default test reporters for HTF.
defaultTestReporters :: IsParallel
                     -> IsJsonOutput
                     -> IsXmlOutput
                     -> [TestReporter]
defaultTestReporters inParallel forMachine doXml =
    case (inParallel, forMachine) of
      (NonParallel, NoJsonOutput) ->
          [TestReporter
           { tr_id = "rep_seq_human"
           , tr_reportAllTests = reportAllTestsH
           , tr_reportGlobalStart = reportGlobalStartHS
           , tr_reportTestStart = reportTestStartHS
           , tr_reportTestResult = reportTestResultHS
           , tr_reportGlobalResults = reportGlobalResultsH
           }] ++ xmlReporters
      (Parallel, NoJsonOutput) ->
          [TestReporter
           { tr_id = "rep_par_human"
           , tr_reportAllTests = reportAllTestsH
           , tr_reportGlobalStart = reportGlobalStartHP
           , tr_reportTestStart = reportTestStartHP
           , tr_reportTestResult = reportTestResultHP
           , tr_reportGlobalResults = reportGlobalResultsH
           }] ++ xmlReporters
      (NonParallel, JsonOutput) ->
          [TestReporter
           { tr_id = "rep_seq_machine"
           , tr_reportAllTests = reportAllTestsM
           , tr_reportGlobalStart = reportGlobalStartMS
           , tr_reportTestStart = reportTestStartMS
           , tr_reportTestResult = reportTestResultMS
           , tr_reportGlobalResults = reportGlobalResultsM
           }] ++ xmlReporters
      (Parallel, JsonOutput) ->
          [TestReporter
           { tr_id = "rep_par_machine"
           , tr_reportAllTests = reportAllTestsM
           , tr_reportGlobalStart = reportGlobalStartMP
           , tr_reportTestStart = reportTestStartMP
           , tr_reportTestResult = reportTestResultMP
           , tr_reportGlobalResults = reportGlobalResultsM
           }] ++ xmlReporters
    where
      xmlReporters =
          case doXml of
            NoXmlOutput -> []
            XmlOutput -> [(emptyTestReporter "rep_xml") {
                            tr_reportGlobalResults = reportGlobalResultsXml
                          }]

--
-- output for humans
--

humanTestName :: GenFlatTest a -> String
humanTestName ft =
    flatName (ft_path ft) ++
    case ft_location ft of
      Nothing -> ""
      Just loc -> " (" ++ showLoc loc ++ ")"

reportHumanTestStartMessage :: ReportLevel -> GenFlatTest a -> TR ()
reportHumanTestStartMessage level ft =
    do let t = colorize testStartColor "[TEST] "
       reportTR level (t +++ noColor (humanTestName ft))

-- sequential
reportGlobalStartHS :: ReportGlobalStart
reportGlobalStartHS _ = return ()

reportTestStartHS :: ReportTestStart
reportTestStartHS ft = reportHumanTestStartMessage Debug ft

reportTestResultHS :: ReportTestResult
reportTestResultHS ftr =
    let res = rr_result (ft_payload ftr)
        msg = attachCallStack (rr_message (ft_payload ftr)) (rr_stack (ft_payload ftr))
    in case res of
         Pass ->
             reportMessage Debug msg okSuffix
         Pending ->
             do reportHumanTestStartMessageIfNeeded
                reportMessage Info msg pendingSuffix
         Fail ->
             do reportHumanTestStartMessageIfNeeded
                reportMessage Info msg failureSuffix
         Error ->
             do reportHumanTestStartMessageIfNeeded
                reportMessage Info msg errorSuffix
   where
     reportHumanTestStartMessageIfNeeded =
         do tc <- ask
            when (tc_quiet tc) (reportHumanTestStartMessage Info ftr)
     reportMessage level msg suffix =
         reportTR level (ensureNewlineColorString msg +++ suffix +++ noColor timeStr)
     timeStr = " (" ++ show (rr_wallTimeMs (ft_payload ftr)) ++ "ms)\n"
     failureSuffix = colorize warningColor "*** Failed!"
     errorSuffix = colorize warningColor "@@@ Error!"
     pendingSuffix = colorize pendingColor "^^^ Pending!"
     okSuffix = colorize testOkColor  "+++ OK"

-- parallel
reportGlobalStartHP :: ReportGlobalStart
reportGlobalStartHP _ = return ()

reportTestStartHP :: ReportTestStart
reportTestStartHP ft =
     do reportStringTR Debug ("Starting " ++ (humanTestName ft))

reportTestResultHP :: ReportTestResult
reportTestResultHP ftr =
    do reportHumanTestStartMessage Debug ftr
       reportTestResultHS ftr

-- results and all tests
reportAllTestsH :: ReportAllTests
reportAllTestsH l =
    reportStringTR Info (render (renderTestNames l))

reportGlobalResultsH :: ReportGlobalResults
reportGlobalResultsH arg =
    do let passed = length (rgra_passed arg)
           pending = length (rgra_pending arg)
           failed = length (rgra_failed arg)
           error = length (rgra_errors arg)
           timedOut = length (rgra_timedOut arg)
           filtered = length (rgra_filtered arg)
           total = passed + failed + error + pending
       let pendings = (if pending > 0 then colorize pendingColor else noColor) "* Pending:"
           failures = (if failed > 0 then colorize warningColor else noColor) "* Failures:"
           errors = (if error > 0 then colorize warningColor else noColor) "* Errors:"
       reportTR Info ("* Tests:     " +++ showC total +++ "\n" +++
                      "* Passed:    " +++ showC passed +++ "\n" +++
                      pendings +++ "   " +++ showC pending +++ "\n" +++
                      failures +++ "  " +++ showC failed +++ "\n" +++
                      errors +++ "    " +++ showC error +++ "\n" +++
                      "* Timed out: " +++ showC timedOut +++ "\n" +++
                      "* Filtered:  " +++ showC filtered)
       when (timedOut > 0) $
            if timedOut < 10
            then
                reportTR Info
                    ("\n" +++ noColor "* Timed out:" +++ "\n" +++ renderTestNames' (reverse (rgra_timedOut arg)))
            else
                reportTR Info
                  ("\n" +++ noColor "* Timed out: (" +++ showC timedOut +++ noColor ", too many to list)")
       when (filtered > 0) $
            if filtered < 10
            then
                reportTR Info
                  ("\n" +++ noColor "* Filtered:" +++ "\n" +++ renderTestNames' (reverse (rgra_filtered arg)))
            else
                reportTR Info
                  ("\n" +++ noColor "* Filtered: (" +++ showC filtered +++ noColor ", too many to list)")
       when (pending > 0) $
          reportTR Info
              ("\n" +++ pendings +++ "\n" +++ renderTestNames' (reverse (rgra_pending arg)))
       when (failed > 0) $
          reportTR Info
              ("\n" +++ failures +++ "\n" +++ renderTestNames' (reverse (rgra_failed arg)))
       when (error > 0) $
          reportTR Info
              ("\n" +++ errors +++ "\n" +++ renderTestNames' (reverse (rgra_errors arg)))
       reportStringTR Info ("\nTotal execution time: " ++ show (rgra_timeMs arg) ++ "ms")
    where
      showC x = noColor (show x)
      renderTestNames' rrs =
          noColor $ render $ nest 2 $ renderTestNames rrs

renderTestNames :: [GenFlatTest a] -> Doc
renderTestNames l =
    vcat (map (\ft -> text "*" <+>
                      text (humanTestName ft)) l)

--
-- output for machines
--

-- sequential
reportGlobalStartMS :: ReportGlobalStart
reportGlobalStartMS _ = return ()

reportTestStartMS :: ReportTestStart
reportTestStartMS ft =
    let json = mkTestStartEventObj ft (flatName (ft_path ft))
    in reportJsonTR json

reportTestResultMS :: ReportTestResult
reportTestResultMS ftr =
    let json = mkTestEndEventObj ftr (flatName (ft_path ftr))
    in reportJsonTR json

-- parallel
reportGlobalStartMP :: ReportGlobalStart
reportGlobalStartMP _ = return ()

reportTestStartMP :: ReportTestStart
reportTestStartMP = reportTestStartMS

reportTestResultMP :: ReportTestResult
reportTestResultMP = reportTestResultMS

-- results and all tests
reportAllTestsM :: ReportAllTests
reportAllTestsM l =
    let json = mkTestListObj (map (\ft -> (ft, flatName (ft_path ft))) l)
    in reportJsonTR json

reportGlobalResultsM :: ReportGlobalResults
reportGlobalResultsM arg =
    let json = mkTestResultsObj arg
    in reportJsonTR json

reportGlobalResultsXml :: ReportGlobalResults
reportGlobalResultsXml arg =
    do let xml = mkGlobalResultsXml arg
       tc <- ask
       case tc_outputXml tc of
         Just fname -> liftIO $ withFile fname WriteMode $ \h -> BSL.hPut h xml
         Nothing -> liftIO $ BSL.putStr xml

--
-- General reporting routines
--

reportTR :: ReportLevel -> ColorString -> TR ()
reportTR level msg =
    do tc <- ask
       let s = renderColorString msg (tc_useColors tc)
       reportGen tc level (\h -> T.hPutStrLn h s)

reportStringTR :: ReportLevel -> String -> TR ()
reportStringTR level msg =
    do tc <- ask
       reportGen tc level (\h -> hPutStrLn h msg)

reportLazyBytesTR :: ReportLevel -> BSL.ByteString -> TR ()
reportLazyBytesTR level msg =
    do tc <- ask
       reportGen tc level (\h -> BSL.hPut h msg)

reportJsonTR :: HTFJsonObj a => a -> TR ()
reportJsonTR x = reportLazyBytesTR Info (decodeObj x)

data ReportLevel = Debug | Info
                 deriving (Eq,Ord)

reportGen :: TestConfig -> ReportLevel -> (Handle -> IO ()) -> TR ()
reportGen tc level fun =
    unless (tc_quiet tc && level < Info) $
    case tc_output tc of
      TestOutputHandle h _ -> liftIO (fun h)
      TestOutputSplitted fp ->
          do -- split mode: one file for each result to avoid locking on windows
             ix <- gets ts_index
             let realFp = fp ++ (show ix) -- just append the index at the end of the file given as output parameter
             modify (\x -> x { ts_index = ts_index x + 1 })
             liftIO $ withFile realFp WriteMode fun
