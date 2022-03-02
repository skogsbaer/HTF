{-# OPTIONS_GHC -cpp -pgmP "cpphs --layout --hashes --cpp" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}

--
-- Copyright (c) 2009-2014 Stefan Wehr - http://www.stefanwehr.de
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

module Test.Framework.Preprocessor (

    transform, progName, preprocessorTests, TransformOptions(..)

) where

-- import Debug.Trace
import Control.Monad
import Data.Char
import Language.Preprocessor.Cpphs ( runCpphsPass1,
                                     runCpphsPass2,
                                     CpphsOptions(..),
                                     BoolOptions(..),
                                     defaultCpphsOptions,
                                     WordStyle(..),
                                     Posn,
                                     filename,
                                     lineno,
                                     newfile,
                                     tokenise
                                   )
import System.IO ( hPutStrLn, stderr )
#if MIN_VERSION_HUnit(1,4,0)
import Test.HUnit hiding (State)
#else
import Test.HUnit hiding (State, Location)
#endif
import Control.Monad.State.Strict
import qualified Data.List as List
import Data.Maybe

import Test.Framework.Location

_DEBUG_ :: Bool
_DEBUG_ = False

progName :: String
progName = "htfpp"

htfModule :: String
htfModule = "Test.Framework"

mkName varName fullModuleName =
    "htf_" ++
    map (\c -> if c == '.' then '_' else c)
        (fullModuleName ++ "." ++
         (case varName of
            'h':'t':'f':'_':s -> s
            s -> s))

thisModulesTestsFullName :: String -> String
thisModulesTestsFullName = mkName thisModulesTestsName

importedTestListFullName :: String -> String
importedTestListFullName = mkName importedTestListName

thisModulesTestsName :: String
thisModulesTestsName = "htf_thisModulesTests"

importedTestListName :: String
importedTestListName = "htf_importedTests"

nameDefines :: ModuleInfo -> [(String, String)]
nameDefines info =
    [(thisModulesTestsName, thisModulesTestsFullName (mi_moduleNameWithDefault info)),
     (importedTestListName, importedTestListFullName (mi_moduleNameWithDefault info))]

allAsserts :: [String]
allAsserts = []

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

data ModuleInfo = ModuleInfo { mi_htfPrefix  :: String
                             , mi_htfImports :: [ImportDecl]
                             , mi_defs       :: [Definition]
                             , mi_moduleName :: Maybe String }
                  deriving (Show, Eq)

mi_moduleNameWithDefault :: ModuleInfo -> String
mi_moduleNameWithDefault = fromMaybe "Main" . mi_moduleName

data ImportDecl = ImportDecl { imp_moduleName :: Name
                             , imp_qualified :: Bool
                             , imp_alias :: Maybe Name
                             , imp_loc :: Location }
                  deriving (Show, Eq)

data Definition = TestDef String Location String
                | PropDef String Location String
                  deriving (Eq, Show)

type Name = String

type PMA a = State ModuleInfo a

setModName :: String -> PMA ()
setModName name =
    do oldName <- gets mi_moduleName
       when (isNothing oldName) $ modify $ \mi -> mi { mi_moduleName = Just name }

addTestDef :: String -> String -> Location -> PMA ()
addTestDef name fullName loc =
    modify $ \mi -> mi { mi_defs = (TestDef name loc fullName) : mi_defs mi }

addPropDef :: String -> String -> Location -> PMA ()
addPropDef name fullName loc =
    modify $ \mi -> mi { mi_defs = (PropDef name loc fullName) : mi_defs mi }

addHtfImport :: ImportDecl -> PMA ()
addHtfImport decl =
    modify $ \mi -> mi { mi_htfImports = decl : mi_htfImports mi }

setTestFrameworkImport :: String -> PMA ()
setTestFrameworkImport name =
    modify $ \mi -> mi { mi_htfPrefix = name }

data Tok
    = TokModule
    | TokQname Location String
    | TokName Location Bool String
    | TokHtfImport Location
    | TokImport Location

transWordStyles :: [WordStyle] -> [Tok]
transWordStyles styles = loop styles True
    where
      loop styles startOfLine =
        case styles of
          [] -> []
          Ident pos name : rest ->
              case name of
                "module" -> TokModule : loop rest False
                "import" ->
                    case dropWhite rest of
                      Other "{-@ HTF_TESTS @-}" : rest2 ->
                          TokHtfImport (posToLocation pos) : loop rest2 False
                      _ ->
                          TokImport (posToLocation pos) : loop rest False
                _ ->
                    case parseQname rest of
                      ([], rest2) ->
                          TokName (posToLocation pos) startOfLine name : loop rest2 False
                      (nameParts, rest2) ->
                          TokQname (posToLocation pos) (List.intercalate "." (name:nameParts)) : loop rest2 False
          Other str : rest ->
              let startOfLine =
                      case reverse str of
                        '\n':_ -> True
                        _ -> False
              in loop rest startOfLine
          Cmd _ : rest -> loop rest False
      dropWhite styles =
          case styles of
            Other str : rest ->
                case dropWhile isSpace str of
                  [] -> dropWhite rest
                  str' -> Other str' : rest
            _ -> styles
      parseQname styles =
          case styles of
            Other "." : Ident _ name : rest ->
                let (restParts, rest2) = parseQname rest
                in (name:restParts, rest2)
            _ -> ([], styles)
      posToLocation pos = makeLoc (filename pos) (lineno pos)

poorManAnalyzeTokens :: [WordStyle] -> ModuleInfo
poorManAnalyzeTokens styles =
    let toks = transWordStyles styles
        revRes =
            execState (loop toks) $
                      ModuleInfo { mi_htfPrefix = htfModule ++ "."
                                 , mi_htfImports = []
                                 , mi_defs = []
                                 , mi_moduleName = Nothing }
    in ModuleInfo { mi_htfPrefix = mi_htfPrefix revRes
                  , mi_htfImports = reverse (mi_htfImports revRes)
                  , mi_defs = reverse $ List.nubBy defEqByName (mi_defs revRes)
                  , mi_moduleName = mi_moduleName revRes
                  }
    where
      defEqByName (TestDef n1 _ _) (TestDef n2 _ _) = n1 == n2
      defEqByName (PropDef n1 _ _) (PropDef n2 _ _) = n1 == n2
      defEqByName _ _ = False
      loop toks =
        case toks of
          TokModule : TokQname _ name : rest ->
              do setModName name
                 loop rest
          TokModule : TokName _ _ name : rest ->
              do setModName name
                 loop rest
          TokName loc startOfLine name : rest
              | startOfLine ->
                  case name of
                    't':'e':'s':'t':'_':shortName ->
                        do addTestDef shortName name loc
                           loop rest
                    'p':'r':'o':'p':'_':shortName ->
                        do addPropDef shortName name loc
                           loop rest
                    _ -> loop rest
              | otherwise -> loop rest
          TokHtfImport loc : rest ->
              case parseImport loc rest of
                Just (imp, rest2) ->
                    do addHtfImport imp
                       loop rest2
                Nothing -> loop rest
          TokImport loc : rest ->
              do case parseImport loc rest of
                   Nothing -> loop rest
                   Just (imp, rest2) ->
                       do when (imp_moduleName imp == htfModule) $
                            let prefix = case (imp_alias imp, imp_qualified imp) of
                                           (Just alias, True) -> alias
                                           (Nothing, True) -> imp_moduleName imp
                                           _ -> ""
                            in setTestFrameworkImport
                                   (if null prefix then prefix else prefix ++ ".")
                          loop rest2
          _ : rest -> loop rest
          [] -> return ()
      parseImport loc toks =
          do let (qualified, toks2) =
                  case toks of
                    TokName _ _ "qualified" : rest -> (True, rest)
                    _ -> (False, toks)
             (name, toks3) <-
                  case toks2 of
                    TokName _ _ name : rest -> return (name, rest)
                    TokQname _ name : rest -> return (name, rest)
                    _ -> fail "no import"
             let (mAlias, toks4) =
                   case toks3 of
                     TokName _ _ "as" : TokName _ _ alias : rest -> (Just alias, rest)
                     _ -> (Nothing, toks3)
                 decl = ImportDecl { imp_moduleName = name
                                   , imp_qualified = qualified
                                   , imp_alias = mAlias
                                   , imp_loc = loc }
             return (decl, toks4)

analyze :: FilePath -> String -> IO (ModuleInfo, [WordStyle], [(Posn,String)])
analyze originalFileName input =
    do xs <- runCpphsPass1 cpphsOptions originalFileName input
       let bopts = boolopts cpphsOptions
           toks = tokenise (stripEol bopts) (stripC89 bopts) (ansi bopts) (lang bopts) ((newfile "preDefined",""):xs)
           mi = poorManAnalyzeTokens toks
       return (mi, toks, xs)

analyzeTests =
    [(unlines ["module FOO where"
              ,"import Test.Framework"
              ,"import {-@ HTF_TESTS @-} qualified Foo as Bar"
              ,"import {-@ HTF_TESTS @-} qualified Foo.X as Egg"
              ,"import {-@ HTF_TESTS @-} Foo.Y as Spam"
              ,"import {-@ HTF_TESTS @-} Foo.Z"
              ,"import {-@ HTF_TESTS @-} Baz"
              ,"deriveSafeCopy 1 'base ''T"
              ,"$(deriveSafeCopy 2 'extension ''T)"
              ,"test_blub test_foo = 1"
              ,"test_blah test_foo = '\''"
              ,"prop_abc prop_foo = 2"
              ,"prop_xyz = True"]
     ,ModuleInfo { mi_htfPrefix = ""
                 , mi_htfImports =
                     [ImportDecl { imp_moduleName = "Foo"
                                 , imp_qualified = True
                                 , imp_alias = Just "Bar"
                                 , imp_loc = makeLoc "<input>" 3}
                     ,ImportDecl { imp_moduleName = "Foo.X"
                                 , imp_qualified = True
                                 , imp_alias = Just "Egg"
                                 , imp_loc = makeLoc "<input>" 4}
                     ,ImportDecl { imp_moduleName = "Foo.Y"
                                 , imp_qualified = False
                                 , imp_alias = Just "Spam"
                                 , imp_loc = makeLoc "<input>" 5}
                     ,ImportDecl { imp_moduleName = "Foo.Z"
                                 , imp_qualified = False
                                 , imp_alias = Nothing
                                 , imp_loc = makeLoc "<input>" 6}
                     ,ImportDecl { imp_moduleName = "Baz"
                                 , imp_qualified = False
                                 , imp_alias = Nothing
                                 , imp_loc = makeLoc "<input>" 7}]
                 , mi_moduleName = Just "FOO"
                 , mi_defs = [TestDef "blub" (makeLoc "<input>" 10) "test_blub"
                             ,TestDef "blah" (makeLoc "<input>" 11) "test_blah"
                             ,PropDef "abc" (makeLoc "<input>" 12) "prop_abc"
                             ,PropDef "xyz" (makeLoc "<input>" 13) "prop_xyz"]
                 })
    ,(unlines ["module Foo.Bar where"
              ,"import Test.Framework as Blub"
              ,"prop_xyz = True"]
     ,ModuleInfo { mi_htfPrefix = ""
                 , mi_htfImports = []
                 , mi_moduleName = Just "Foo.Bar"
                 , mi_defs = [PropDef "xyz" (makeLoc "<input>" 3) "prop_xyz"]
                 })
    ,(unlines ["module Foo.Bar where"
              ,"import qualified Test.Framework as Blub"
              ,"prop_xyz = True"]
     ,ModuleInfo { mi_htfPrefix = "Blub."
                 , mi_htfImports = []
                 , mi_moduleName = Just "Foo.Bar"
                 , mi_defs = [PropDef "xyz" (makeLoc "<input>" 3) "prop_xyz"]
                 })
    ,(unlines ["module Foo.Bar where"
              ,"import qualified Test.Framework"
              ,"prop_xyz = True"]
     ,ModuleInfo { mi_htfPrefix = "Test.Framework."
                 , mi_htfImports = []
                 , mi_moduleName = Just "Foo.Bar"
                 , mi_defs = [PropDef "xyz" (makeLoc "<input>" 3) "prop_xyz"]
                 })]

testAnalyze =
    do mapM_ runTest (zip [1..] analyzeTests)
    where
      runTest (i, (src, mi)) =
          do (givenMi, _, _) <- analyze "<input>" src
             if givenMi == mi
             then return ()
             else assertFailure ("Error in test " ++ show i ++
                                 ", expected:\n" ++ show mi ++
                                 "\nGiven:\n" ++ show givenMi ++
                                 "\nSrc:\n" ++ src)

cpphsOptions :: CpphsOptions
cpphsOptions =
    defaultCpphsOptions {
      boolopts = (boolopts defaultCpphsOptions) { lang = True } -- lex as haskell
    }

data TransformOptions = TransformOptions { hunitBackwardsCompat :: Bool
                                         , debug :: Bool
                                         , literateTex :: Bool }

transform :: TransformOptions -> FilePath -> String -> IO String
transform (TransformOptions hunitBackwardsCompat debug literateTex) originalFileName input =
    do (info, toks, pass1) <- analyze originalFileName fixedInput
       preprocess info toks pass1
    where
      preprocess info toks pass1 =
          do when debug $
                  do hPutStrLn stderr ("Tokens: " ++ show toks)
                     hPutStrLn stderr ("Module info:\n" ++ show info)
             let opts = mkOptionsForModule info
             preProcessedInput <-
                 runCpphsPass2 (boolopts opts) (defines opts) originalFileName pass1
             return $ preProcessedInput ++ "\n\n" ++ possiblyWrap literateTex (additionalCode info) ++ "\n"
      -- fixedInput serves two purposes:
      -- 1. add a trailing \n
      -- 2. turn lines of the form '# <number> "<filename>"' into line directives '#line <number> <filename>'
      -- (see http://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html#Preprocessor-Output).
      fixedInput :: String
      fixedInput = (unlines . map fixLine . lines) input
          where
            fixLine s =
                case parseCppLineInfoOut s of
                  Just (line, fileName) -> "#line " ++ line ++ " " ++ fileName
                  _ -> s
      mkOptionsForModule :: ModuleInfo -> CpphsOptions
      mkOptionsForModule info =
          defaultCpphsOptions { defines =
                                    defines defaultCpphsOptions ++
                                    assertDefines hunitBackwardsCompat (mi_htfPrefix info) ++
                                    nameDefines info
                              , boolopts = (boolopts defaultCpphsOptions) { lang = True } -- lex as haskell
                              }
      possiblyWrap :: Bool -> String -> String
      possiblyWrap b s = if b then "\\begin{code}\n" ++ s ++ "\\end{code}" else s
      additionalCode :: ModuleInfo -> String
      additionalCode info =
          thisModulesTestsFullName (mi_moduleNameWithDefault info) ++ " :: " ++
            mi_htfPrefix info ++ "TestSuite\n" ++
          thisModulesTestsFullName (mi_moduleNameWithDefault info) ++ " = " ++
            mi_htfPrefix info ++ "makeTestSuite" ++
          " " ++ show (mi_moduleNameWithDefault info) ++
          " [\n" ++ List.intercalate ",\n"
                          (map (codeForDef (mi_htfPrefix info)) (mi_defs info))
          ++ "\n  ]\n" ++ importedTestListCode info
      codeForDef :: String -> Definition -> String
      codeForDef pref (TestDef s loc name) =
          locPragma loc ++ pref ++ "makeUnitTest " ++ (show s) ++ " " ++ codeForLoc pref loc ++
          " " ++ name
      codeForDef pref (PropDef s loc name) =
          locPragma loc ++ pref ++ "makeQuickCheckTest " ++ (show s) ++ " " ++
          codeForLoc pref loc ++ " (" ++ pref ++ "qcAssertion " ++ name ++ ")"
      locPragma :: Location -> String
      locPragma loc =
          "{-# LINE " ++ show (lineNumber loc) ++ " " ++ show (fileName loc) ++ " #-}\n    "
      codeForLoc :: String -> Location -> String
      codeForLoc pref loc = "(" ++ pref ++ "makeLoc " ++ show (fileName loc) ++
                            " " ++ show (lineNumber loc) ++ ")"
      importedTestListCode :: ModuleInfo -> String
      importedTestListCode info =
          let l = mi_htfImports info
          in case l of
               [] -> ""
               _ -> (importedTestListFullName (mi_moduleNameWithDefault info)
                     ++ " :: [" ++ mi_htfPrefix info ++ "TestSuite]\n" ++
                     importedTestListFullName (mi_moduleNameWithDefault info)
                     ++ " = [\n    " ++
                     List.intercalate ",\n     " (map htfTestsInModule l) ++
                     "\n  ]\n")
      htfTestsInModule :: ImportDecl -> String
      htfTestsInModule imp = qualify imp (thisModulesTestsFullName (imp_moduleName imp))
      qualify :: ImportDecl -> String -> String
      qualify imp name =
          case (imp_qualified imp, imp_alias imp) of
            (False, _) -> name
            (True, Just alias) -> alias ++ "." ++ name
            (True, _) -> imp_moduleName imp ++ "." ++ name

-- Returns for lines of the form '# <number> "<filename>"'
-- (see http://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html#Preprocessor-Output)
-- the value 'Just <number> "<filename>"'
parseCppLineInfoOut :: String -> Maybe (String, String)
parseCppLineInfoOut line =
    case line of
      '#':' ':c:rest
        | isDigit c ->
            case List.span isDigit rest of
              (restDigits, ' ' : '"' : rest) ->
                  case dropWhile (/= '"') (reverse rest) of
                    '"' : fileNameRev ->
                        let line = (c:restDigits)
                            file = "\"" ++ reverse fileNameRev ++ "\""
                        in Just (line, file)
                    _ -> Nothing
              _ -> Nothing
      _ -> Nothing

preprocessorTests =
    [("testAnalyze", testAnalyze)]
