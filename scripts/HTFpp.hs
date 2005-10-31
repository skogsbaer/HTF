module Main where

import IO
import System
import Process

macros = unlines
         [ "#define SRC_LOC_ (__FILE__, __LINE__)"
         , "#define assert (assert_ SRC_LOC_)"
         , "#define assertEqual (assertEqual_ SRC_LOC_)"
         , "#define assertEqual2 (assertEqual2_ SRC_LOC_)"
         , "#define assertSeqEqual (assertSeqEqual_ SRC_LOC_)"
         , "#define assertNull (assertNull_ SRC_LOC_)"
         , "#define assertNotNull (assertNotNull_ SRC_LOC_)" ]

main = 
    do prog <- getProgName
       args <- getArgs
       if length args /= 4 
          then hPutStrLn stderr ("Usage: " ++ prog ++ 
                                 " original-filename input-filename output-filename "
                                 ++ "cpp-command")
          else let orig = args!!0
                   infile = args!!1
                   outfile = args!!2
                   cpp = args!!3
                   fstLine = "#line 1 \"" ++ orig ++ "\"\n"
                   in do inString <- readFile infile
                         (out,err,_) <- popen cpp ["-w"] (Just $ macros ++ fstLine ++
                                                                 inString)
                         if null err 
                            then do writeFile outfile out
                                    exitWith ExitSuccess
                            else do hPutStrLn stderr err
                                    exitWith (ExitFailure 1)

