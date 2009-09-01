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
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Extension ( glasgowExts )

import Test.Framework.Location

progName :: String
progName = "htfpp"

htfModule :: String
htfModule = "Test.Framework"

testDeclName :: String
testDeclName = "allHTFTests"

data Definition = TestDef String Location String 
                | PropDef String Location String

assertDefines :: String -> [(String, String)]
assertDefines prefix =
    map (\s -> (s, "(" ++ prefix ++ s ++ "_ (" ++ 
                   prefix ++ "makeLoc __FILE__ __LINE__))"))
        ["assertBool"
        ,"assertEqual"
        ,"assertEqualNoShow"
        ,"assertSetEqual"
        ,"assertEmpty"
        ,"assertNotEmpty"
        ,"assertThrows"
        ,"assertThrowsSome"
        ,"assertLeft"
        ,"assertLeftNoShow"
        ,"assertRight"
        ,"assertRightNoShow"
        ,"assertJust"
        ]

warn :: String -> IO ()
warn s = 
    hPutStrLn stderr $ progName ++ " warning: " ++ s

nameAsString :: Name -> String
nameAsString (Ident s) = s
nameAsString (Symbol s) = s

data ModuleInfo = ModuleInfo { mi_prefix     :: String
                             , mi_defs       :: [Definition]
                             , mi_moduleName :: String }

analyse :: FilePath -> String 
        -> IO (ParseResult ModuleInfo)
analyse originalFileName s = 
    let parseResult = parseModuleWithMode parseMode s
    in case parseResult of
         ParseOk (Module loc (ModuleName moduleName) 
                         pragmas maybeWarning maybeExports 
                         imports decls) ->
             do -- putStrLn $ show decls
                let defs = mapMaybe defFromDecl decls
                htfPrefix <-
                  case mapMaybe prefixFromImport imports of
                    (s:_) -> return s
                    [] -> do warn ("No import found for " ++ htfModule ++ " in "
                                   ++ originalFileName)
                             return (htfModule ++ ".")
                return $ ParseOk (ModuleInfo htfPrefix defs moduleName)
         ParseFailed loc err -> return (ParseFailed loc err)
    where
      parseMode :: ParseMode
      parseMode = defaultParseMode { parseFilename = originalFileName
                                   , extensions = glasgowExts }
      prefixFromImport :: ImportDecl -> Maybe String
      prefixFromImport (ImportDecl loc (ModuleName s) qualified _ _
                                   alias _) 
          | s == htfModule =
              if qualified
                  then case alias of
                         Just (ModuleName s') -> Just $ s' ++ "."
                         Nothing -> Just $ s ++ "."
                  else Just ""
      prefixFromImport _ = Nothing
      defFromDecl :: Decl -> Maybe Definition
      defFromDecl (PatBind loc (PVar name) _ _ _) = 
          defFromNameAndLoc name loc
      defFromDecl (FunBind (Match loc name _ _ _ _ : _)) =
          defFromNameAndLoc name loc
      defFromDecl _ = Nothing
      defFromNameAndLoc :: Name -> SrcLoc -> Maybe Definition
      defFromNameAndLoc name loc =
          let l = makeLoc (srcFilename loc) (srcLine loc)
              s = nameAsString name
          in case s of
               ('t':'e':'s':'t':'_':rest) | not (null rest) -> 
                   Just (TestDef rest l s)
               ('p':'r':'o':'p':'_':rest) | not (null rest) -> 
                   Just (PropDef rest l s)
               _ -> Nothing

transform :: FilePath -> String -> IO String
transform originalFileName input =
    do analyseResult <- analyse originalFileName 
                          -- the parser fails if the last line is a line
                          -- comment not ending with \n
                          (input ++ "\n")
       case analyseResult of
         ParseFailed loc err ->
             do warn ("Parsing of " ++ originalFileName ++ " failed at line " 
                      ++ show (srcLine loc) ++ ", column " ++ 
                      show (srcColumn loc) ++ ": " ++ err)
                return (preprocess (ModuleInfo "" [] "UNKNOWN_MODULE"))
         ParseOk info ->
             return (preprocess info)
    where
      preprocess :: ModuleInfo -> String
      preprocess info = 
          let preProcessedInput = runCpphs (cpphsOptions info) 
                                           originalFileName input
          in preProcessedInput ++ "\n\n" ++ additionalCode info ++ "\n"
      cpphsOptions :: ModuleInfo -> CpphsOptions
      cpphsOptions info = 
          defaultCpphsOptions { defines = 
                                    defines defaultCpphsOptions ++
                                            assertDefines (mi_prefix info)
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
