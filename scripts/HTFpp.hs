module Main where

import IO
import System
import Process

assertions = [ "assertBool"
             , "assertEqual"
             , "assertEqualNoShow"
             , "assertSetEqual"
             , "assertNull"
             , "assertNotNull"
             , "assertThrows"
             ]

macros = unlines $
         "#define SRC_LOC_ (__FILE__, __LINE__)" :
         map define assertions
    where define s = "#define " ++ s ++ " (" ++ s ++ "_ SRC_LOC_)"

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

