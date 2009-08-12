{-# LANGUAGE TemplateHaskell #-}

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

module Test.Framework (

  module HU, module QC, module BBT, module Syn, module TM,

  Loc.makeLoc

) where

import Test.Framework.HUnitWrapper as HU
import Test.Framework.QuickCheckWrapper as QC
import Test.Framework.BlackBoxTest as BBT
import Test.Framework.Syntax as Syn
import Test.Framework.TestManager as TM
import qualified Test.Framework.Location as Loc

          