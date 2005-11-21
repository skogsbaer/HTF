{-# OPTIONS -fth #-}

-- 
-- Copyright (c) 2005   Stefan Wehr - http://www.stefanwehr.de
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

  module HU, module QC, module FBT,

  tests

) where

import Data.Maybe
import Language.Haskell.TH

import Test.Framework.HUnitWrapper as HU
import Test.Framework.QuickCheckWrapper as QC
import Test.Framework.FileBasedTest as FBT

tests :: String -> Q [Dec] -> Q [Dec]
tests name decs = 
    do decs' <- decs
       -- runIO $ putStrLn (show decs')
       moduleName <- currentModule
       let ts = collectTests decs'
           props = collectProps decs'
           testName = moduleName ++ "." ++ name
       e <- [| HU.TestLabel testName 
                 (HU.TestList $(listE (map mkExp ts ++ 
                                       map (mkPropExp testName) props))) |]
       let lete = LetE decs' e
           suiteDec = ValD (VarP (mkName name)) (NormalB lete) []
           resDecs = [suiteDec]
       -- runIO $ putStrLn (show props) --(pprint resDecs)
       return resDecs
    where
    collectTests :: [Dec] -> [Name]
    collectTests = mapMaybe f 
        where f (ValD (VarP name) _ _) | isTestName (nameBase name) = Just name
              f _ = Nothing 
    collectProps :: [Dec] -> [(Name, String, Bool)]
    collectProps = mapMaybe f
        where f (ValD (VarP name) _ _) = analyzePropName name
              f (FunD name _) = analyzePropName name
              f _ = Nothing 
    isTestName :: String -> Bool
    isTestName ('t':'e':'s':'t':'_':s) | not (null s) = True
    isTestName _ = False
    analyzePropName :: Name -> Maybe (Name, String, Bool)
    analyzePropName name =
        case nameBase name of
          ('p':'r':'o':'p':'_':'c':'f':'g':'_':s) 
              | not (null s) -> Just (name, s, True)
          ('p':'r':'o':'p':'_':s) 
              | not (null s) -> Just (name, s, False)
          _ -> Nothing
    mkExp :: Name -> Q Exp
    mkExp name = 
        let s = nameBase name
            in [| HU.TestLabel s (HU.TestCase $(varE name)) |]
    mkPropExp :: String -> (Name, String, Bool) -> Q Exp
    mkPropExp testName (name, s, customCfg) =
        let fullName = testName `joinPathElems` s
            exp = if customCfg then [| $(varE name) |]
                  else [| (id, $(varE name)) |]
            in [| HU.TestLabel s 
                    (HU.TestCase (testableAsAssertion fullName $(exp))) |]
