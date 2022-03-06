--
-- Copyright (c) 2005,2009   Stefan Wehr - http://www.stefanwehr.de
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

{- |

Top-level module that re-exports functionality from sub-modules.
Modules that only define unit tests and quickcheck properties typically
only need to import this module.
Test drivers should additionally import 'Test.Framework.TestManager' and,
if needed, 'Test.Framework.BlackBoxTest'.

See "Test.Framework.Tutorial" for a description how to use HTF.

-}
module Test.Framework (

  -- * Unit tests
  module Test.Framework.HUnitWrapper, TM.makeUnitTest,

  -- * Quickcheck
  module Test.Framework.QuickCheckWrapper, TM.makeQuickCheckTest,

  -- * Generic assertions
  module Test.Framework.AssertM,

  -- * Organizing tests
  TM.makeTestSuite, TM.TestSuite, TM.htfMain, TM.htfMainWithArgs, Loc.makeLoc, TM.runTest,
  TM.WrappableHTF(..)

) where

import Test.Framework.HUnitWrapper
import Test.Framework.QuickCheckWrapper
import Test.Framework.AssertM
import qualified Test.Framework.TestManager as TM
import qualified Test.Framework.Location as Loc
