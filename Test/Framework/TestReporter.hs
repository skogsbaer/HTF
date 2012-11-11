module Test.Framework.TestReporter where

import Test.Framework.TestTypes
import Test.Framework.Location
import Test.Framework.Colors
import Test.Framework.Utils

import System.IO
import Control.Monad.RWS
import Text.PrettyPrint

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

reportAllTests :: ReportAllTests
reportAllTests tests =
    do reps <- asks tc_reporters
       mapM_ (\r -> tr_reportAllTests r tests) reps

reportGlobalStart :: ReportGlobalStart
reportGlobalStart tests =
    do reps <- asks tc_reporters
       mapM_ (\r -> tr_reportGlobalStart r tests) reps

reportTestStart :: ReportTestStart
reportTestStart t =
    do reps <- asks tc_reporters
       mapM_ (\r -> tr_reportTestStart r t) reps

reportTestResult :: ReportTestResult
reportTestResult t msg =
    do reps <- asks tc_reporters
       mapM_ (\r -> tr_reportTestResult r t msg) reps

reportGlobalResults :: ReportGlobalResults
reportGlobalResults l1 l2 l3 l4 =
    do reps <- asks tc_reporters
       mapM_ (\r -> tr_reportGlobalResults r l1 l2 l3 l4) reps

defaultTestReporters :: Bool -- ^ 'True' if tests are run in parallel
                     -> Bool -- ^ 'True' if machine output should be produced
                     -> [TestReporter]
defaultTestReporters inParallel forMachine =
    case (inParallel, forMachine) of
      (False, False) ->
          [TestReporter
           { tr_id = "rep_seq_human"
           , tr_reportAllTests = reportAllTestsH
           , tr_reportGlobalStart = reportGlobalStartHS
           , tr_reportTestStart = reportTestStartHS
           , tr_reportTestResult = reportTestResultHS
           , tr_reportGlobalResults = reportGlobalResultsH
           }]
      (True, False) ->
          [TestReporter
           { tr_id = "rep_par_human"
           , tr_reportAllTests = reportAllTestsH
           , tr_reportGlobalStart = reportGlobalStartHP
           , tr_reportTestStart = reportTestStartHP
           , tr_reportTestResult = reportTestResultHP
           , tr_reportGlobalResults = reportGlobalResultsH
           }]
      x -> error ("no reporter defined for combination " ++ show x)

--
-- output for humans
--

humanTestName :: TestID -> Maybe Location -> String
humanTestName id mloc =
    id ++ case mloc of
            Nothing -> ""
            Just loc -> " (" ++ showLoc loc ++ ")"

reportHumanTestStartMessage :: ReportLevel -> TestID -> Maybe Location -> TR ()
reportHumanTestStartMessage level id mloc =
    do t <- liftIO $ colorize testStartColor "[TEST] "
       reportTR level (t ++ (humanTestName id mloc))

-- sequential

reportGlobalStartHS :: ReportGlobalStart
reportGlobalStartHS _ = return ()

reportTestStartHS :: ReportTestStart
reportTestStartHS ft = reportHumanTestStartMessage Debug (ft_id ft) (ft_location ft)

reportTestResultHS :: ReportTestResult
reportTestResultHS rr msg =
   case rr_result rr of
     Pass ->
         do pref <- okPrefix
            reportMessage Debug msg pref
     Pending ->
         do reportHumanTestStartMessageIfNeeded
            pref <- pendingPrefix
            reportMessage Info msg pref
     Fail ->
         do reportHumanTestStartMessageIfNeeded
            pref <- failurePrefix
            reportMessage Info msg pref
     Error ->
         do reportHumanTestStartMessageIfNeeded
            pref <- errorPrefix
            reportMessage Info msg pref
   where
     reportHumanTestStartMessageIfNeeded =
         do tc <- ask
            when (tc_quiet tc) (reportHumanTestStartMessage Info (rr_id rr) (rr_location rr))
     reportMessage level msg prefix =
         reportTR level (ensureNewline msg ++ prefix)
     failurePrefix = liftIO $ colorize warningColor "*** Failed!\n"
     errorPrefix = liftIO $ colorize warningColor "@@@ Error!\n"
     pendingPrefix = liftIO $ colorize pendingColor "^^^ Pending!\n"
     okPrefix = liftIO $ colorize testOkColor  "+++ OK\n"

-- parallels

reportGlobalStartHP :: ReportGlobalStart
reportGlobalStartHP _ = return ()

reportTestStartHP :: ReportTestStart
reportTestStartHP ft =
     do reportTR Debug ("Starting " ++ (humanTestName (ft_id ft) (ft_location ft)))

reportTestResultHP :: ReportTestResult
reportTestResultHP rr msg =
    do reportHumanTestStartMessage Debug (rr_id rr) (rr_location rr)
       reportTestResultHS rr msg

-- results

reportAllTestsH :: ReportAllTests
reportAllTestsH l =
    reportDoc Info (renderTestNames (map (\ft -> (ft_id ft, ft_location ft)) l))

reportGlobalResultsH :: ReportGlobalResults
reportGlobalResultsH passedL pendingL failedL errorL =
    do let passed = length passedL
           pending = length pendingL
           failed = length failedL
           error = length errorL
           total = passed + failed + error + pending
       pendings <- liftIO $ colorize pendingColor "* Pending:"
       failures <- liftIO $ colorize warningColor "* Failures:"
       errors <- liftIO $ colorize warningColor "* Errors:"
       reportTR Info ("* Tests:    " ++ show total ++ "\n" ++
                      "* Passed:   " ++ show passed ++ "\n" ++
                      pendings ++ "  " ++ show pending ++ "\n" ++
                      failures ++ " " ++ show failed ++ "\n" ++
                      errors ++ "   " ++ show error )
       when (pending > 0) $
          reportDoc Info
              (text ('\n' : pendings) $$ renderTestNames' (reverse pendingL))
       when (failed > 0) $
          reportDoc Info
              (text ('\n' : failures) $$ renderTestNames' (reverse failedL))
       when (error > 0) $
          reportDoc Info
              (text ('\n' : errors) $$ renderTestNames' (reverse errorL))
    where
      renderTestNames' rrs =
          nest 2 $ renderTestNames $ map (\rr -> (rr_id rr, rr_location rr)) rrs

renderTestNames :: [(TestID, Maybe Location)] -> Doc
renderTestNames l =
    vcat (map (\(tid, loc) -> text "*" <+>
                              text (humanTestName tid loc)) l)


--
-- General reporting routines
--

reportDoc :: ReportLevel -> Doc -> TR ()
reportDoc level doc = reportTR level (render doc)

reportTR :: ReportLevel -> String -> TR ()
reportTR level msg =
    do tc <- ask
       liftIO $ report tc level msg

reportBytesTR :: ReportLevel -> BS.ByteString -> TR ()
reportBytesTR level msg =
    do tc <- ask
       liftIO $ reportBytes tc level msg

reportLazyBytesTR :: ReportLevel -> BSL.ByteString -> TR ()
reportLazyBytesTR level msg =
    do tc <- ask
       liftIO $ reportLazyBytes tc level msg

data ReportLevel = Debug | Info
                 deriving (Eq,Ord)

report :: TestConfig -> ReportLevel -> String -> IO ()
report tc level msg = reportGen tc level (\h -> hPutStrLn h msg)

reportBytes :: TestConfig -> ReportLevel -> BS.ByteString -> IO ()
reportBytes tc level msg = reportGen tc level (\h -> BS.hPut h msg)

reportLazyBytes :: TestConfig -> ReportLevel -> BSL.ByteString -> IO ()
reportLazyBytes tc level msg = reportGen tc level (\h -> BSL.hPut h msg)

reportGen :: TestConfig -> ReportLevel -> (Handle -> IO ()) -> IO ()
reportGen tc level fun =
    unless (tc_quiet tc && level < Info) $ do let h = tc_outputHandle tc
                                              fun h
                                              hFlush h
