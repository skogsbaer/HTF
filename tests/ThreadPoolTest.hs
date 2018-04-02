--
-- Copyright (c) 2013   Stefan Wehr - http://www.stefanwehr.de
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

import Test.Framework.ThreadPool
import System.Environment
import System.Exit
import Control.Monad

main :: IO ()
main =
    do args <- getArgs
       when ("-h" `elem` args || "--help" `elem` args) usage
       (i, nEntries) <- case args of
                          [] -> return (200, 100)
                          [x] -> return (read x, 100)
                          [x, y] -> return (read x, read y)
                          _ -> usage
       threadPoolTest (1, i) nEntries
       return ()
    where
      usage =
          do putStrLn "USAGE: ThreadPoolTest [N_THREADS [N_ENTRIES]]"
             exitWith (ExitFailure 1)
