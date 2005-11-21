module Test.Framework.FileBasedTest ( 
  
  Diff, FBTConfig(..), 

  defaultFBTConfig, defaultDiff,

  fileBasedTests

) where

import Prelude hiding ( catch )

import System.IO
import System.Exit
import Control.Exception
import System.Directory 
import Data.List ( mapAccumL )
import qualified Data.Map as Map
import Control.Monad

import Test.Framework.Process
import Test.Framework.HUnitWrapper as HU
import Test.Framework.Utils

type Diff = Maybe FilePath      -- Name of the file that contains the expected output.
                                -- If the parameter is Nothing, then no output
                                -- is expected.
          -> String             -- Actual output
          -> IO (Maybe String)  -- A Nothing value means ok, otherwise the
                                -- Just value wraps the error message

data FileBasedTest = FileBasedTest
                   { fbt_shouldFail  :: Bool
                   , fbt_cmd         :: String
                   , fbt_stdinFile   :: Maybe FilePath
                   , fbt_stdoutFile  :: Maybe FilePath
                   , fbt_stderrFile  :: Maybe FilePath
                     -- functions for comparing output on stdout and stderr.
                   , fbt_stdoutCmp   :: Diff
                   , fbt_stderrCmp   :: Diff
                   }

runFileBasedTest :: FileBasedTest -> HU.Assertion
runFileBasedTest fbt = 
    do inp <- case fbt_stdinFile fbt of
                Nothing -> return Nothing
                Just f -> do s <- readFile f
                             return $ Just s
       (out,err,exit) <- popenShell (fbt_cmd fbt) inp
       case exit of
         ExitSuccess | fbt_shouldFail fbt 
           -> HU.assertFailure ("test is supposed to fail but succeeded")
         ExitFailure i | not $ fbt_shouldFail fbt
           -> do hPutStrLn stderr $ "stderr for " ++ show (fbt_cmd fbt) ++ ":"
                 hPutStr stderr err
                 putStrLn $ "stdout for " ++ show (fbt_cmd fbt) ++ ":"
                 putStr out
                 HU.assertFailure ("test is supposed to succeed but failed with " ++
                                "exit code " ++ show i)
         _ -> do cmpOut <- cmp (fbt_stdoutFile fbt) (fbt_stdoutCmp fbt)
                             out "Mismatch on stdout:\n"
                 cmpErr <- cmp (fbt_stderrFile fbt) (fbt_stderrCmp fbt)
                             err "Mismatch on stderr:\n"
                 case (cmpOut, cmpErr) of
                  (Nothing, Nothing) -> return ()
                  (x1, x2) -> HU.assertFailure (x1 `concatMaybes` x2)
    where cmp expectFile cmpAction real label = 
              do res <- cmpAction expectFile real
                 case res of
                   Nothing -> return Nothing
                   Just s -> return $ Just (label ++ s)
          concatMaybes Nothing Nothing = ""
          concatMaybes (Just s) Nothing = s
          concatMaybes (Nothing) (Just s) = s
          concatMaybes (Just s1) (Just s2) = s1 ++ "\n" ++ s2


data FBTConfig = FBTConfig { fbt_stdinSuffix    :: String
                           , fbt_stdoutSuffix   :: String
                           , fbt_stderrSuffix   :: String
                           , fbt_dynConfigName  :: String
                           , fbt_stdoutDiff     :: Diff
                           , fbt_stderrDiff     :: Diff }

defaultDiff :: Diff
defaultDiff expectFile real = 
    do mexe <- findExecutable "diff"
       let exe = case mexe of
                   Just p -> p
                   Nothing -> error ("diff command not in path")
       case expectFile of
         Nothing | null real -> return Nothing
                 | otherwise -> return $ Just ("no output expected, but given:\n" 
                                               ++ real)
         Just expect ->
             do (out, err, exitCode) <- popen exe ["-u", expect, "-"] (Just real)
                case exitCode of
                  ExitSuccess -> return Nothing       -- no difference
                  ExitFailure 1 -> return $ Just out  -- files differ
                  ExitFailure i -> error ("diff command failed with exit code " ++ 
                                          show i ++ ": " ++ err)

defaultFBTConfig = FBTConfig { fbt_stdinSuffix    = ".in"
                             , fbt_stdoutSuffix   = ".out"
                             , fbt_stderrSuffix   = ".err"
                             , fbt_dynConfigName  = "FBTConfig"
                             , fbt_stdoutDiff     = defaultDiff
                             , fbt_stderrDiff     = defaultDiff }

fileBasedTests :: String    -- id for the tests
               -> FilePath  -- root directory of the test hierarchy
               -> String    -- name of executable
               -> String    -- filename suffix for input file
               -> FBTConfig -- configuration
               -> IO HU.Test
fileBasedTests id root exe suf cfg = 
    do let prune root _ = do dynCfg <- readDynCfg Map.empty
                                                  (root </> fbt_dynConfigName cfg)
                             return $ dyn_skip dynCfg
       inputFiles <- collectFiles root suf prune
       (_, tests) <- mapAccumLM genTest Map.empty inputFiles
       return $ HU.TestLabel id $ HU.TestList tests
    where genTest :: DynamicConfigMap -> FilePath -> IO (DynamicConfigMap, HU.Test)
          genTest map fname =
            do stdinf <- maybeFile $ replaceSuffix fname (fbt_stdinSuffix cfg)
               stdoutf <- maybeFile $  replaceSuffix fname (fbt_stdoutSuffix cfg)
               stderrf <- maybeFile $ replaceSuffix fname (fbt_stderrSuffix cfg)
               let configFile = dirname fname </> fbt_dynConfigName cfg
               dynCfg <- readDynCfg map configFile
               let cmd = exe ++ " " ++ dropSpace (dyn_flags dynCfg) ++ " " ++ fname
                   shouldFail = dyn_shouldFail dynCfg
               let fbt = FileBasedTest
                         { fbt_shouldFail  = shouldFail
                         , fbt_cmd         = cmd
                         , fbt_stdinFile   = stdinf
                         , fbt_stdoutFile  = stdoutf
                         , fbt_stderrFile  = stderrf
                         , fbt_stdoutCmp   = fbt_stdoutDiff cfg
                         , fbt_stderrCmp   = fbt_stderrDiff cfg
                         }
               return (Map.insert configFile dynCfg map,
                       HU.TestLabel fname $ HU.TestCase $ runFileBasedTest fbt)

data DynamicConfig = DynamicConfig { dyn_skip        :: Bool
                                   , dyn_flags       :: String
                                   , dyn_shouldFail  :: Bool }

type DynamicConfigMap = Map.Map FilePath DynamicConfig

defaultDynCfg = DynamicConfig False "" False

readDynCfg :: DynamicConfigMap -> FilePath -> IO DynamicConfig
readDynCfg m f = 
    do case Map.lookup f m of
         Just dynCfg -> return dynCfg
         Nothing ->
             do b <- doesFileExist f
                if not b then return $ defaultDynCfg
                   else do s <- readFile f
                           return $ foldl (parse f) defaultDynCfg $ 
                                 filter (not . isUseless) (map dropSpace (lines s))
    where isUseless :: String -> Bool
          isUseless []      = True
          isUseless ('#':_) = True
          isUseless _       = False
          parse :: FilePath -> DynamicConfig -> String -> DynamicConfig
          parse _ cfg "Skip" = cfg { dyn_skip = True }
          parse _ cfg "Fail" = cfg { dyn_shouldFail = True }
          parse _ cfg ('F':'l':'a':'g':'s':':':flags) = cfg { dyn_flags = flags }
          parse f _ l = error ("invalid line in dynamic configuration file `" ++
                               f ++ "': " ++ show l)
    