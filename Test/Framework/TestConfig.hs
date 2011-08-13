--
-- Copyright (c) 2009-2011   Stefan Wehr - http://www.stefanwehr.de
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

module Test.Framework.TestConfig (
  TestConfig(..), defaultTestConfig, ReportLevel(..), report
) where

import Control.Monad (unless)

data TestConfig = TestConfig { tc_quiet :: Bool }
                deriving (Read,Show,Eq)

defaultTestConfig :: TestConfig
defaultTestConfig = TestConfig { tc_quiet = False }

data ReportLevel = Debug | Info
                 deriving (Eq,Ord)

report :: TestConfig -> ReportLevel -> String -> IO ()
report tc level msg =
    unless (tc_quiet tc && level < Info) $ putStrLn msg
