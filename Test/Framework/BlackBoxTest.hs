-- 
-- Copyright (c) 2005,2009   Stefan Wehr - http://www.stefanwehr.de
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

A /black box test/ in the terminology of the HTF consists of a
driver program that is run in various input files. For each input
file, the HTF checks that the driver program exits with the
correct exit code and that it produces the expected output.

-}
module Test.Framework.BlackBoxTest ( 
  
  BBTArgs(..), defaultBBTArgs, 

  blackBoxTests,

  Diff, defaultDiff

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
import Test.Framework.TestManager
import Test.Framework.Utils

{- | 
The type of a function comparing the content of a file
against a string, similar to the unix tool @diff@.
The first parameter is the name of the file containing the 
expected output. If this parameter is 'Nothing', then no output
is expected. The second parameter is the actual output produced.
If the result is 'Nothing' then no difference was found. 
Otherwise, a 'Just' value contains a string explaining the
different.
-}
type Diff = Maybe FilePath -> String -> IO (Maybe String)

data BlackBoxTestCfg = BlackBoxTestCfg
                       { bbtCfg_shouldFail  :: Bool
                       , bbtCfg_cmd         :: String
                       , bbtCfg_stdinFile   :: Maybe FilePath
                       , bbtCfg_stdoutFile  :: Maybe FilePath
                       , bbtCfg_stderrFile  :: Maybe FilePath
                       , bbtCfg_verbose     :: Bool
                       -- functions for comparing output on stdout and stderr.
                       , bbtCfg_stdoutCmp   :: Diff
                       , bbtCfg_stderrCmp   :: Diff
                       }

runBlackBoxTest :: BlackBoxTestCfg -> Assertion
runBlackBoxTest bbt = 
    do inp <- case bbtCfg_stdinFile bbt of
                Nothing -> return Nothing
                Just f -> do s <- readFile f
                             return $ Just s
       (out,err,exit) <- popenShell (bbtCfg_cmd bbt) inp
       case exit of
         ExitSuccess | bbtCfg_shouldFail bbt 
           -> blackBoxTestFail ("test is supposed to fail but succeeded")
         ExitFailure i | not $ bbtCfg_shouldFail bbt
           -> do when (bbtCfg_verbose bbt) $
                   do hPutStrLn stderr ("stderr for " ++ show (bbtCfg_cmd bbt) 
                                        ++ ":")
                      hPutStrLn stderr (err ++ (endOfOutput "output"))
                      putStrLn $ "stdout for " ++ show (bbtCfg_cmd bbt) ++ ":"
                      putStrLn (out ++ (endOfOutput "output"))
                 blackBoxTestFail ("test is supposed to succeed but failed "
                                   ++ "with exit code " ++ show i)
         _ -> do cmpOut <- cmp (bbtCfg_stdoutFile bbt) (bbtCfg_stdoutCmp bbt)
                             out "Mismatch on stdout:\n"
                 cmpErr <- cmp (bbtCfg_stderrFile bbt) (bbtCfg_stderrCmp bbt)
                             err "Mismatch on stderr:\n"
                 case (cmpOut, cmpErr) of
                  (Nothing, Nothing) -> return ()
                  (x1, x2) -> 
                      do when (bbtCfg_verbose bbt) $ 
                              putStrLn (x1 `concatMaybes` x2)
                         let mismatchOn = 
                                 case (cmpOut, cmpErr) of
                                   (Just _, Just _) -> "stdout and stderr"
                                   (Just _, Nothing) -> "stdout"
                                   _ -> "stderr"
                         blackBoxTestFail ("Mismatch on " ++ mismatchOn)
    where cmp expectFile cmpAction real label = 
              do res <- cmpAction expectFile real
                 case res of
                   Nothing -> return Nothing
                   Just s -> return $ Just (label ++ s)
          concatMaybes Nothing Nothing = ""
          concatMaybes (Just s) Nothing = s
          concatMaybes (Nothing) (Just s) = s
          concatMaybes (Just s1) (Just s2) = s1 ++ "\n" ++ s2

endOfOutput :: String -> String
endOfOutput s = "[end of " ++ s ++ "]"

data BBTArgs = BBTArgs { bbtArgs_stdinSuffix    :: String
                       , bbtArgs_stdoutSuffix   :: String
                       , bbtArgs_stderrSuffix   :: String
                       , bbtArgs_dynArgsName    :: String
                       , bbtArgs_verbose        :: Bool
                       , bbtArgs_stdoutDiff     :: Diff
                       , bbtArgs_stderrDiff     :: Diff }

defaultBBTArgs :: BBTArgs
defaultBBTArgs = BBTArgs { bbtArgs_stdinSuffix    = ".in"
                         , bbtArgs_stdoutSuffix   = ".out"
                         , bbtArgs_stderrSuffix   = ".err"
                         , bbtArgs_dynArgsName    = "BBTArgs"
                         , bbtArgs_stdoutDiff     = defaultDiff
                         , bbtArgs_stderrDiff     = defaultDiff
                         , bbtArgs_verbose        = False }

defaultDiff :: Diff
defaultDiff expectFile real = 
    do mexe <- findExecutable "diff"
       let exe = case mexe of
                   Just p -> p
                   Nothing -> error ("diff command not in path")
       case expectFile of
         Nothing | null real -> return Nothing
                 | otherwise -> return $ Just ("no output expected, but " ++
                                               "given:\n" ++ real ++
                                               (endOfOutput "given output"))
         Just expect ->
             do (out, err, exitCode) <- popen exe ["-u", expect, "-"] 
                                          (Just real)
                case exitCode of
                  ExitSuccess -> return Nothing       -- no difference
                  ExitFailure 1 ->                    -- files differ
                      return $ Just (out ++ (endOfOutput "diff output"))
                  ExitFailure i -> error ("diff command failed with exit " ++
                                          "code " ++ show i ++ ": " ++ err)

blackBoxTests :: FilePath  -- root directory of the test hierarchy
              -> String    -- name of executable
              -> String    -- filename suffix for input file
              -> BBTArgs   -- configuration
              -> IO [Test]
blackBoxTests root exe suf cfg =     
    do let prune root _ = do dynCfg <- readDynCfg Map.empty
                                                  (root </> 
                                                   bbtArgs_dynArgsName cfg)
                             return $ dyn_skip dynCfg
       inputFiles <- collectFiles root suf prune
       (_, tests) <- mapAccumLM genTest Map.empty inputFiles
       return tests
    where genTest :: DynamicConfigMap -> FilePath -> IO (DynamicConfigMap, 
                                                         Test)
          genTest map fname =
            do stdinf <- maybeFile $ replaceSuffix fname 
                                       (bbtArgs_stdinSuffix cfg)
               stdoutf <- maybeFile $  replaceSuffix fname 
                                         (bbtArgs_stdoutSuffix cfg)
               stderrf <- maybeFile $ replaceSuffix fname 
                                        (bbtArgs_stderrSuffix cfg)
               let configFile = dirname fname </> bbtArgs_dynArgsName cfg
               dynCfg <- readDynCfg map configFile
               let cmd = exe ++ " " ++ dropSpace (dyn_flags dynCfg) ++ " " ++ 
                         fname
                   shouldFail = dyn_shouldFail dynCfg
                   verbose = bbtArgs_verbose cfg || dyn_verbose dynCfg
               let bbt = BlackBoxTestCfg
                         { bbtCfg_shouldFail  = shouldFail
                         , bbtCfg_cmd         = cmd
                         , bbtCfg_stdinFile   = stdinf
                         , bbtCfg_stdoutFile  = stdoutf
                         , bbtCfg_stderrFile  = stderrf
                         , bbtCfg_verbose     = verbose
                         , bbtCfg_stdoutCmp   = bbtArgs_stdoutDiff cfg
                         , bbtCfg_stderrCmp   = bbtArgs_stderrDiff cfg
                         }
               return (Map.insert configFile dynCfg map,
                       makeBlackBoxTest fname (runBlackBoxTest bbt))

data DynamicConfig = DynamicConfig { dyn_skip        :: Bool
                                   , dyn_flags       :: String
                                   , dyn_shouldFail  :: Bool
                                   , dyn_verbose     :: Bool }

type DynamicConfigMap = Map.Map FilePath DynamicConfig

defaultDynCfg = DynamicConfig { dyn_skip       = False
                              , dyn_flags      = ""
                              , dyn_shouldFail = False
                              , dyn_verbose    = False }

readDynCfg :: DynamicConfigMap -> FilePath -> IO DynamicConfig
readDynCfg m f = 
    do case Map.lookup f m of
         Just dynCfg -> return dynCfg
         Nothing ->
             do b <- doesFileExist f
                if not b then return $ defaultDynCfg
                   else do s <- readFile f
                           return $ foldl (parse f) defaultDynCfg $ 
                                 filter (not . isUseless) (map dropSpace 
                                                               (lines s))
    where isUseless :: String -> Bool
          isUseless []      = True
          isUseless ('#':_) = True
          isUseless _       = False
          parse :: FilePath -> DynamicConfig -> String -> DynamicConfig
          parse _ cfg "Skip" = cfg { dyn_skip = True }
          parse _ cfg "Fail" = cfg { dyn_shouldFail = True }
          parse _ cfg "Verbose" = cfg { dyn_verbose = True }
          parse _ cfg ('F':'l':'a':'g':'s':':':flags) = cfg { dyn_flags = flags }
          parse f _ l = error ("invalid line in dynamic configuration file `" ++
                               f ++ "': " ++ show l)
    