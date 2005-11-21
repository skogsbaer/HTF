{-# OPTIONS -cpp #-}
-- 
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

-- stolen from Yi

--
-- | A Posix.popen compatibility mapping.
-- Based on PosixCompat, originally written by Derek Elkins for lambdabot
--
module Test.Framework.Process ( popen, popenShell ) where

import System.IO
import System.Process
import System.Exit
import Control.Concurrent       (forkIO)
import qualified Control.Exception

popenShell :: String -> Maybe String -> IO (String,String,ExitCode)
popenShell cmd = popen' $ runInteractiveCommand cmd

popen :: FilePath -> [String] -> Maybe String -> IO (String,String,ExitCode)
popen file args = 
    popen' $ runInteractiveProcess file args Nothing Nothing

popen' :: IO (Handle, Handle, Handle, ProcessHandle) 
       -> Maybe String
       -> IO (String,String,ExitCode)
popen' run minput = 
    Control.Exception.handle (\e -> return ([],show e,error (show e))) $ do

    (inp,out,err,pid) <- run

    case minput of
        Just input -> hPutStr inp input >> hClose inp -- importante!
        Nothing    -> return ()

    -- Now, grab the input
    output <- hGetContents out
    errput <- hGetContents err

    -- SimonM sez:
    --  ... avoids blocking the main thread, but ensures that all the
    --  data gets pulled as it becomes available. you have to force the
    --  output strings before waiting for the process to terminate.
    --
    forkIO (Control.Exception.evaluate (length output) >> return ())
    forkIO (Control.Exception.evaluate (length errput) >> return ())

    -- And now we wait. We must wait after we read, unsurprisingly.
    ecode <- waitForProcess pid -- blocks without -threaded, you're warned.

    -- so what's the point of returning the pid then?
    return (output,errput,ecode)

