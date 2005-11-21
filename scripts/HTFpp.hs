-- 
-- Copyright (c) 2005   Stefan Wehr - http://www.stefanwehr.de
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
-- 

module Main where

import IO
import System
import Test.Framework.Process

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
          then hPutStrLn stderr 
                   ("Usage: " ++ prog ++ 
                    " original-filename input-filename output-filename "
                    ++ "cpp-command")
          else let orig = args!!0
                   infile = args!!1
                   outfile = args!!2
                   cpp = args!!3
                   fstLine = "#line 1 \"" ++ orig ++ "\"\n"
                   in do inString <- readFile infile
                         (out,err,_) <- popen cpp ["-w"] 
                                        (Just $ macros ++ fstLine ++
                                                inString)
                         if null err 
                            then do writeFile outfile out
                                    exitWith ExitSuccess
                            else do hPutStrLn stderr err
                                    exitWith (ExitFailure 1)

