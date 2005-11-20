{-

DarcsPatchInfo: retrieve information on the number of patches and the
                timestamp of the latest patch in a darcs repository.

Author: Stefan Wehr (http://www.stefanwehr.de)

History:

 * 2005-11-20: created

License:

Copyright (c) 2005, Stefan Wehr
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module DarcsPatchInfo (

  DarcsPatchInfo(..), getDarcsPatchInfo, isUnderDarcsControl, formatDarcsPatchInfo

) where

import System.IO
import System.Process
import System.Exit
import Control.Exception ( evaluate )
import System.Directory
import Control.Monad
import Text.Regex

--
-- Public interface
--

data DarcsPatchInfo = DarcsPatchInfo 
                    { dpi_patchCount   :: Int
                    , dpi_latestPatch  :: String }
                    deriving (Eq,Show)

getDarcsPatchInfo :: IO DarcsPatchInfo
isUnderDarcsControl :: IO Bool
formatDarcsPatchInfo :: DarcsPatchInfo -> String

--
-- Internal stuff
--

darcsCmd = "darcs changes --xml-output"

runCmd :: String -> IO (ExitCode, 
                        String,   -- stdout
                        String)   -- stderr
runCmd cmd = 
    do (inh, outh, errh, pid) <- runInteractiveCommand cmd

       outp <- hGetContents outh
       errp <- hGetContents errh
       hClose inh

       -- SimonM sez:
       -- ... avoids blocking the main thread, but ensures that all the
       -- data gets pulled as it becomes available. you have to force the
       -- output strings before waiting for the process to terminate.
       --
       Control.Exception.evaluate (length outp)
       Control.Exception.evaluate (length errp)

       exitCode <- waitForProcess pid
       return (exitCode, outp, errp)

darcsChanges = 
    do (e, out, err) <- runCmd darcsCmd
       case e of
         ExitSuccess -> return out
         _ -> error ("darcs failed with exit code " ++ show e ++ ": " ++ err)

countPatches s = 
    foldl count 0 (lines s)
    where count n s = n + (if s == "</patch>" then 1 else 0)

patchDate :: String -> Maybe String
patchDate s = 
    case matchRegex patchDateRe s of
      Just (x:_) -> Just x
      _          -> Nothing
    where patchDateRe = mkRegex "<patch .*date='([0-9]+)'"

getDarcsPatchInfo = 
    do s <- darcsChanges
       case msum (map patchDate $ lines s) of
         Just d -> return $ DarcsPatchInfo (countPatches s) d
         Nothing -> error ("cannot detect date of latest patch")

isUnderDarcsControl = doesDirectoryExist "_darcs"

formatDarcsPatchInfo dpi = 
    (take 8 $ dpi_latestPatch dpi) ++ "-p" ++ show (dpi_patchCount dpi)
