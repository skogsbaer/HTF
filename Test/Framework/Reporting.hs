module Test.Framework.Reporting where

import Control.Monad.RWS
import Text.PrettyPrint

import Test.Framework.TestTypes
import Test.Framework.TestConfig
import Test.Framework.Location
import Test.Framework.Colors
import Test.Framework.Utils

{-

Output strategies:

- For humans, parallel (default): no redirections
- For humans, sequential: no redirections
- For machines, parallel: no redirections, write HTF output to a file
- For machines, sequential: no redirections, write HTF output to a file,
  and markers to stdout and stderr before running a test

-}

reportGlobalStart :: [FlatTest] -> TR ()
reportGlobalStart l =
    do tc <- ask
       case () of
         _| (not (tc_machineOutput tc) && not (tc_parallel tc)) -> reportAllTestsHS l
         _| (not (tc_machineOutput tc) && tc_parallel tc) -> reportAllTestsHP l

reportTestStart :: FlatTest -> TR ()
reportTestStart ft =
    do tc <- ask
       case () of
         _| (not (tc_machineOutput tc) && not (tc_parallel tc)) -> reportTestStartHS ft
         _| (not (tc_machineOutput tc) && tc_parallel tc) -> reportTestStartHP ft

reportTestResult :: RunResult -> String -> TR ()
reportTestResult x msg =
    do tc <- ask
       case () of
         _| (not (tc_machineOutput tc) && not (tc_parallel tc)) -> reportTestResultHS x msg
         _| (not (tc_machineOutput tc) && tc_parallel tc) -> reportTestResultHP x msg

reportGlobalResults :: [RunResult] -> [RunResult] -> [RunResult] -> [RunResult] -> TR ()
reportGlobalResults pass pending failed error =
    do tc <- ask
       case () of
         _| not (tc_machineOutput tc) -> reportGlobalResultsH pass pending failed error

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

reportAllTestsHS _ = return ()

reportTestStartHS ft = reportHumanTestStartMessage Debug (ft_id ft) (ft_location ft)

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

-- parallel
reportAllTestsHP _ = return ()

reportTestStartHP ft =
     do reportTR Debug ("Starting " ++ (humanTestName (ft_id ft) (ft_location ft)))

reportTestResultHP rr msg =
    do reportHumanTestStartMessage Debug (rr_id rr) (rr_location rr)
       reportTestResultHS rr msg

-- results
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
              (text ('\n' : pendings) $$ renderTestNames (reverse pendingL))
       when (failed > 0) $
          reportDoc Info
              (text ('\n' : failures) $$ renderTestNames (reverse failedL))
       when (error > 0) $
          reportDoc Info
              (text ('\n' : errors) $$ renderTestNames (reverse errorL))
    where
      renderTestNames l =
          nest 2 (vcat (map (\rr -> text "*" <+>
                                    text (humanTestName (rr_id rr) (rr_location rr))) l))


--
-- General reporting routines
--

reportDoc :: ReportLevel -> Doc -> TR ()
reportDoc level doc = reportTR level (render doc)

reportTR :: ReportLevel -> String -> TR ()
reportTR level msg =
    do tc <- ask
       liftIO $ report tc level msg
