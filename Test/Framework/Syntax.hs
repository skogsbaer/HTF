{-# LANGUAGE TemplateHaskell #-}

-- 
-- Copyright (c) 2009   Stefan Wehr - http://www.stefanwehr.de
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

module Test.Framework.Syntax ( tests ) where

import Data.Maybe
import Language.Haskell.TH
import Test.Framework.TestManager
import Test.Framework.QuickCheckWrapper ( testableAsAssertion, 
                                          asTestableWithArgs )

{-
foo :: Q [Dec] -> Q [Dec]
foo decs = 
    do decs' <- decs
       dds <- mapM rewriteDec decs'
       return (concat dds)
    where rewriteDec d@(ValD (VarP name) _ _) = 
              do e <- [| testableAsAssertion' (asTestableWithArgs $(varE name)) |]
                 return [d, ValD (VarP (mkName (nameBase name ++ "Assertion"))) 
                                 (NormalB e) []]
-}

tests :: String -> Q [Dec] -> Q [Dec]
tests name decs = 
    do decs' <- decs
       -- runIO $ putStrLn (show decs')
       loc <- location
       let moduleName = loc_module loc
       let ts = collectTests decs'
           props = collectProps decs'
           testName = moduleName ++ "." ++ name
       e <- [| makeTestSuite testName 
                 $(listE (map mkExp ts ++ 
                          map (mkPropExp testName) props)) |]
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
    collectProps :: [Dec] -> [(Name, String)]
    collectProps = mapMaybe f
        where f (ValD (VarP name) _ _) = analyzePropName name
              f (FunD name _) = analyzePropName name
              f _ = Nothing 
    isTestName :: String -> Bool
    isTestName ('t':'e':'s':'t':'_':s) | not (null s) = True
    isTestName _ = False
    analyzePropName :: Name -> Maybe (Name, String)
    analyzePropName name =
        case nameBase name of
          ('p':'r':'o':'p':'_':s) | not (null s) -> Just (name, s)
          _ -> Nothing
    mkExp :: Name -> Q Exp
    mkExp name = 
        let s = nameBase name
            in [| makeUnitTest s $(varE name) |]
    mkPropExp :: String -> (Name, String) -> Q Exp
    mkPropExp testName (name, s) =
        [| makeQuickCheckTest s
                    (testableAsAssertion (asTestableWithArgs $(varE name))) |]