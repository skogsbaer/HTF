module Main where

-- A simple main program. The program reads the contents of the
-- given input file, checks whether the content is a number, and
-- then ouputs the results of apply funA and funB to this number.

import MyPkg.A
import MyPkg.B

import Test.Framework
import System.Environment (getArgs)
import System.Exit
import System.IO

main =
    do args <- getArgs
       case args of
         ["-h"] -> usage
         ["--help"] -> usage
         [file] -> doWork file
         _ -> usage
    where
      usage =
          do hPutStrLn stderr "Usage: sample [-h|--help] FILE"
             exitWith (ExitFailure 1)
      doWork file =
          do s <- readFile file
             case reads s of
               [(n, "")] ->
                   do putStrLn $ show (funA n)
                      putStrLn $ show (funB n)
                      exitWith ExitSuccess
               _ -> do hPutStrLn stderr "invalid input"
                       exitWith (ExitFailure 1)
