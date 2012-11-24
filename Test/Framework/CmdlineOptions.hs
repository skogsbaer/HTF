{-# LANGUAGE CPP #-}
--
-- Copyright (c) 2009-2012   Stefan Wehr - http://www.stefanwehr.de
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

module Test.Framework.CmdlineOptions (

    CmdlineOptions(..), defaultCmdlineOptions, parseTestArgs, helpString,
    testConfigFromCmdlineOptions

) where

import Test.Framework.TestReporter
import Test.Framework.TestTypes
import Test.Framework.Colors
import Test.Framework.Utils

import Data.Char (toLower)
import Data.Maybe

import Control.Monad (unless)
import System.IO

import System.Console.GetOpt
import qualified Text.Regex as R

#ifndef mingw32_HOST_OS
import System.Posix.Terminal
import System.Posix.IO (stdOutput)
import System.Posix.Types (Fd)
import System.Posix.Env (getEnv)
#endif

#ifdef COMPILER_GHC
import GHC.Conc ( numCapabilities )
#endif

--
-- CmdlineOptions
--

-- | Options for running tests.
data CmdlineOptions = CmdlineOptions {
      opts_quiet :: Bool                -- ^ Be quiet or not.
    , opts_filter :: TestFilter         -- ^ Run only tests matching this filter.
    , opts_help :: Bool                 -- ^ If 'True', display a help message and exit.
    , opts_negated :: [String]          -- ^ Regular expressions matching test names which should /not/ run.
    , opts_threads :: Maybe Int         -- ^ Use @Just i@ for parallel execution with @i@ threads, @Nothing@ for sequential execution
    , opts_machineOutput :: Bool        -- ^ Format output for machines or humans
    , opts_useColors :: Maybe Bool      -- ^ Use @Just b@ to enable/disable use of colors, @Nothing@ inferes the use of colors
    , opts_outputFile :: Maybe FilePath -- ^ The output file, defaults to stdout
    , opts_listTests :: Bool
    }

defaultCmdlineOptions :: CmdlineOptions
{- | The default 'CmdlineOptions'.
-}
defaultCmdlineOptions = CmdlineOptions {
      opts_quiet = False
    , opts_filter = const True
    , opts_help = False
    , opts_negated = []
    , opts_threads = Nothing
    , opts_machineOutput = False
    , opts_useColors = Nothing
    , opts_outputFile = Nothing
    , opts_listTests = False
    }

processorCount :: Int
#ifdef COMPILER_GHC
processorCount = numCapabilities
#else
processorCount = 1
#endif

optionDescriptions :: [OptDescr (CmdlineOptions -> CmdlineOptions)]
optionDescriptions =
    [ Option ['q']     ["quiet"]   (NoArg (\o -> o { opts_quiet = True })) "only display errors"
    , Option ['n']     ["not"]     (ReqArg (\s o -> o { opts_negated = s : (opts_negated o) })
                                           "PATTERN") "tests to exclude"
    , Option ['l']     ["list"]   (NoArg (\o -> o { opts_listTests = True })) "list all matching tests"
--     , Option ['j']     ["threads"]   (OptArg (\ms o -> o { opts_threads = Just (parseThreads ms) }) "N")
--                                      ("run N tests in parallel, default N=" ++ show processorCount)
    , Option ['o']     ["output-file"] (ReqArg (\s o -> o { opts_outputFile = Just s })
                                               "FILE") "name of output file"
    , Option [   ]     ["json"] (NoArg (\o -> o { opts_machineOutput = True }))
                               "output results in machine-readable JSON format"
    , Option []        ["colors"]  (ReqArg (\s o -> o { opts_useColors = Just (parseBool s) })
                                           "BOOL") "use colors or not"
    , Option ['h']     ["help"]    (NoArg (\o -> o { opts_help = True })) "display this message"
    ]
    where
      parseThreads Nothing = processorCount
      parseThreads (Just s) =
          case readM s of
            Just i -> i
            Nothing -> error ("invalid number of threads: " ++ s)
      parseBool s =
          if map toLower s `elem` ["1", "true", "yes", "on"] then True else False

{- |

Parse commandline arguments into 'CmdlineOptions'. Here's a synopsis
of the format of the commandline arguments:

FIXME
-}

parseTestArgs :: [String] -> Either String CmdlineOptions
parseTestArgs args =
    case getOpt Permute optionDescriptions args of
      (optTrans, tests, []  ) ->
          let posStrs = tests
              negStrs = opts_negated opts
              pos = map mkRegex posStrs
              neg = map mkRegex negStrs
              pred (FlatTest _ path _ _) =
                  let flat = flatName path
                  in if (any (\s -> s `matches` flat) neg)
                        then False
                        else null pos || any (\s -> s `matches` flat) pos
              opts = (foldr ($) defaultCmdlineOptions optTrans) { opts_filter = pred }
          in Right opts
      (_,_,errs) ->
          Left (concat errs ++ usageInfo usageHeader optionDescriptions)
    where
      matches r s = isJust $ R.matchRegex r s
      mkRegex s = R.mkRegexWithOpts s True False

usageHeader :: String
usageHeader = ("USAGE: COMMAND [OPTION ...] PATTERN ...\n\n" ++
               "  where PATTERN is a posix regular expression matching\n" ++
               "  the names of the tests to run.\n")

helpString :: String
helpString = usageInfo usageHeader optionDescriptions

--
-- TestConfig
--

testConfigFromCmdlineOptions :: CmdlineOptions -> IO TestConfig
testConfigFromCmdlineOptions opts =
    do (outputHandle, closeOutput, mOutputFd) <- openOutputFile
       colors <- checkColors mOutputFd
       setUseColors colors
       let threads = opts_threads opts
           reporters = defaultTestReporters (isJust threads) (opts_machineOutput opts)
       return $ TestConfig { tc_quiet = opts_quiet opts
                           , tc_threads = threads
                           , tc_outputHandle = outputHandle
                           , tc_closeOutput = closeOutput
                           , tc_reporters = reporters
                           , tc_filter = opts_filter opts }
    where
#ifdef mingw32_HOST_OS
      openOutputFile =
          case opts_outputFile opts of
            Nothing -> return (stdout, False, Nothing)
            Just fname ->
                do f <- openFile fname WriteMode
                   return (f, True, Nothing)
      checkColors mOutputFd =
          case opts_useColors opts of
            Just b -> return b
            Nothing -> return False
#else
      openOutputFile =
          case opts_outputFile opts of
            Nothing -> return (stdout, False, Just stdOutput)
            Just fname ->
                do f <- openFile fname WriteMode
                   return (f, True, Nothing)
      checkColors mOutputFd =
          case opts_useColors opts of
            Just b -> return b
            Nothing ->
                do mterm <- getEnv "TERM"
                   case mterm of
                     Nothing -> return False
                     Just s | map toLower s == "dumb" -> return False
                     _ -> do mx <- getEnv "HTF_NO_COLORS"
                             case mx of
                               Just s | map toLower s `elem` ["", "1", "y", "yes", "true"] -> return False
                               _ -> case mOutputFd of
                                      Just fd -> queryTerminal fd
                                      _ -> return False
#endif
