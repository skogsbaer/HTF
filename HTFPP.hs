{-# LANGUAGE ScopedTypeVariables,CPP #-}

--
-- Copyright (c) 2009   Stefan Wehr - http://www.stefanwehr.de
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

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding ( catch )
#endif
import System.IO
import System.Environment
import System.Exit
import Control.Monad
import Control.Exception
import Data.Version

import Test.Framework.Preprocessor
import qualified Paths_HTF

usage :: IO ()
usage =
    hPutStrLn stderr
      ("Preprocessor for the Haskell Test Framework\n\n" ++
       "Usage: " ++ progName ++ " [--debug|--version|--literate-tex] [FILE1 [FILE2 [FILE3]]]\n\n" ++
       "* If no argument is given, input is read from stdin and\n" ++
       "  output is written to stdout.\n" ++
       "* If only FILE1 is given, input is read from this file\n" ++
       "  and output is written to stdout.\n" ++
       "* If FILE1 and FILE2 are given, input is read from FILE1\n" ++
       "  and output is written to FILE2.\n" ++
       "* If FILE1, FILE2, and FILE3 are given, input is read\n" ++
       "  from FILE2, output is written to FILE3, and\n" ++
       "  FILE1 serves as the original input filename.\n\n" ++
       "The `--hunit' flag has been deprecated starting with version 0.15." ++
       "The `--literate-tex` flag causes the additional generated code to be wrapped\n" ++
       "in TeX-style literate code blocks.")

outputVersion :: IO ()
outputVersion =
    hPutStrLn stderr (showVersion Paths_HTF.version)

saveOpenFile :: FilePath -> IOMode -> IO Handle
saveOpenFile path mode =
    openFile path mode `catch` exHandler
    where
      exHandler :: SomeException -> IO Handle
      exHandler e =
          do hPutStrLn stderr ("Error opening file " ++ path ++ ": " ++
                               show e)
             exitWith (ExitFailure 1)

main =
    do args <- getArgs
       when ("-h" `elem` args ||
             "-help" `elem` args ||
             "--help" `elem` args) $
           do usage
              exitWith (ExitFailure 1)
       when ("--version" `elem` args) $
            do outputVersion
               exitWith ExitSuccess
       let transformOpts = TransformOptions { debug = "--debug" `elem` args
                                            , literateTex = "--literate-tex" `elem` args }
           restArgs = flip filter args $ \x -> not $ x `elem` ["--hunit", "--debug", "--literate-tex"]
       (origInputFilename, hIn, hOut) <-
           case restArgs of
             [] ->
                 return ("<stdin>", stdin, stdout)
             file1:[] ->
                 do h <- saveOpenFile file1 ReadMode
                    return (file1, h, stdout)
             file1:file2:[] ->
                 do h1 <- saveOpenFile file1 ReadMode
                    h2 <- saveOpenFile file2 WriteMode
                    return (file1, h1, h2)
             file1:file2:file3:[] ->
                 do h1 <- saveOpenFile file2 ReadMode
                    h2 <- saveOpenFile file3 WriteMode
                    return (file1, h1, h2)
             _ ->
                 do hPutStrLn stderr ("Too many arguments: " ++ show restArgs)
                    usage
                    exitWith (ExitFailure 1)
       input <- hGetContents hIn
       output <- transform transformOpts origInputFilename input `catch`
                   (\ (e::SomeException) ->
                        do hPutStrLn stderr (progName ++
                                             ": unexpected exception: " ++
                                             show e)
                           return ("#line 1 " ++ show origInputFilename ++
                                   "\n" ++ input))
       hPutStr hOut output
       hFlush hOut
