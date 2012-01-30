{-# LANGUAGE ScopedTypeVariables #-}

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

module Test.Framework.Preprocessor ( transform, progName ) where

import Data.Maybe ( mapMaybe )
import Data.List ( intercalate )
import System.IO ( hPutStrLn, stderr )
import Language.Preprocessor.Cpphs ( runCpphs,
                                     CpphsOptions(..),
                                     defaultCpphsOptions)

import Test.Framework.HaskellParser
import Test.Framework.Location

progName :: String
progName = "htfpp"

htfModule :: String
htfModule = "Test.Framework"

testDeclName :: String
testDeclName = "allHTFTests"

allAsserts :: [String]
allAsserts = ["assertBool"
             ,"assertEqual"
             ,"assertEqualPretty"
             ,"assertEqualNoShow"
             ,"assertNotEqual"
             ,"assertNotEqualPretty"
             ,"assertNotEqualNoShow"
             ,"assertListsEqualAsSets"
             ,"assertEmpty"
             ,"assertNotEmpty"
             ,"assertThrows"
             ,"assertThrowsSome"
             ,"assertLeft"
             ,"assertLeftNoShow"
             ,"assertRight"
             ,"assertRightNoShow"
             ,"assertJust"
             ,"assertNothing"
             ,"assertNothingNoShow"
             ]

assertDefines :: Bool -> String -> [(String, String)]
assertDefines hunitBackwardsCompat prefix =
    concatMap fun allAsserts
    where
      fun a =
          if hunitBackwardsCompat
             then [(a, expansion a "Verbose_"), (a ++ "HTF", expansion a "_")]
             else [(a, expansion a "_"), (a ++ "Verbose", expansion a "Verbose_")]
      expansion a suffix = "(" ++ prefix ++ a ++ suffix ++ " (" ++
                           prefix ++ "makeLoc __FILE__ __LINE__))"

warn :: String -> IO ()
warn s =
    hPutStrLn stderr $ progName ++ " warning: " ++ s

data ModuleInfo = ModuleInfo { mi_prefix     :: String
                             , mi_defs       :: [Definition]
                             , mi_moduleName :: String }

data Definition = TestDef String Location String
                | PropDef String Location String

analyse :: FilePath -> String
        -> IO (ParseResult ModuleInfo)
analyse originalFileName s =
    do parseResult <- parse originalFileName s
       case parseResult of
         ParseOK (Module moduleName imports decls) ->
             do -- putStrLn $ show decls
                let defs = mapMaybe defFromDecl decls
                htfPrefix <-
                  case mapMaybe prefixFromImport imports of
                    (s:_) -> return s
                    [] -> do warn ("No import found for " ++ htfModule ++
                                   " in " ++ originalFileName)
                             return (htfModule ++ ".")
                return $ ParseOK (ModuleInfo htfPrefix defs moduleName)
         ParseError loc err -> return (ParseError loc err)
    where
      prefixFromImport :: ImportDecl -> Maybe String
      prefixFromImport (ImportDecl s qualified alias)
          | s == htfModule =
              if qualified
                  then case alias of
                         Just s' -> Just $ s' ++ "."
                         Nothing -> Just $ s ++ "."
                  else Just ""
      prefixFromImport _ = Nothing
      defFromDecl :: Decl -> Maybe Definition
      defFromDecl (Decl loc name) = defFromNameAndLoc name loc
      defFromNameAndLoc :: Name -> Location -> Maybe Definition
      defFromNameAndLoc name loc =
          case name of
            ('t':'e':'s':'t':'_':rest) | not (null rest) ->
                Just (TestDef rest loc name)
            ('p':'r':'o':'p':'_':rest) | not (null rest) ->
                Just (PropDef rest loc name)
            _ -> Nothing

transform :: Bool -> FilePath -> String -> IO String
transform hunitBackwardsCompat originalFileName input =
    do analyseResult <- analyse originalFileName input
       case analyseResult of
         ParseError loc err ->
             do warn ("Parsing of " ++ originalFileName ++ " failed at line "
                      ++ show (lineNumber loc) ++ ": " ++ err)
                preprocess (ModuleInfo "" [] "UNKNOWN_MODULE")
         ParseOK info ->
             preprocess info
    where
      preprocess :: ModuleInfo -> IO String
      preprocess info =
          do preProcessedInput <- runCpphs (cpphsOptions info) originalFileName
                                           input
             return $ preProcessedInput ++ "\n\n" ++ additionalCode info ++ "\n"
      cpphsOptions :: ModuleInfo -> CpphsOptions
      cpphsOptions info =
          defaultCpphsOptions { defines =
                                    defines defaultCpphsOptions ++
                                            assertDefines hunitBackwardsCompat
                                                          (mi_prefix info)
                              }
      additionalCode :: ModuleInfo -> String
      additionalCode info =
          testDeclName ++ " :: " ++ mi_prefix info ++ "TestSuite\n" ++
          testDeclName ++ " = " ++ mi_prefix info ++ "makeTestSuite" ++
          " " ++ show (mi_moduleName info) ++
          " [\n    " ++ intercalate ",\n    "
                          (map (codeForDef (mi_prefix info)) (mi_defs info))
          ++ "\n  ]"
      codeForDef :: String -> Definition -> String
      codeForDef pref (TestDef s loc name) =
          pref ++ "makeUnitTest " ++ (show s) ++ " " ++ codeForLoc pref loc ++
          " " ++ name
      codeForDef pref (PropDef s loc name) =
          pref ++ "makeQuickCheckTest " ++ (show s) ++ " " ++
          codeForLoc pref loc ++ " (" ++ pref ++ "testableAsAssertion (" ++
          pref ++ "asTestableWithQCArgs " ++ name ++ "))"
      codeForLoc :: String -> Location -> String
      codeForLoc pref loc = "(" ++ pref ++ "makeLoc " ++ show (fileName loc) ++
                            " " ++ show (lineNumber loc) ++ ")"
