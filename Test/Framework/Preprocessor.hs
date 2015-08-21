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

    transform, progName, preprocessorTests

) where

-- import Debug.Trace
import Control.Monad
import Data.Char
import "haskell-lexer" Language.Haskell.Lexer
import Language.Preprocessor.Cpphs ( runCpphs,
                                     CpphsOptions(..),
                                     BoolOptions(..),
                                     defaultCpphsOptions)
import System.IO ( hPutStrLn, stderr )
import Test.HUnit hiding (State, Location)
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
allAsserts =
    withGs ["assertBool"
           ,"assertEqual"
           ,"assertEqualPretty"
           ,"assertEqualNoShow"
           ,"assertNotEqual"
           ,"assertNotEqualPretty"
           ,"assertNotEqualNoShow"
           ,"assertListsEqualAsSets"
           ,"assertElem"
           ,"assertEmpty"
           ,"assertNotEmpty"
           ,"assertLeft"
           ,"assertLeftNoShow"
           ,"assertRight"
           ,"assertRightNoShow"
           ,"assertJust"
           ,"assertNothing"
           ,"assertNothingNoShow"
           ,"subAssert"
           ,"subAssertVerbose"
           ] ++ ["assertThrows"
                ,"assertThrowsSome"
                ,"assertThrowsIO"
                ,"assertThrowsSomeIO"
                ,"assertThrowsM"
                ,"assertThrowsSomeM"]
    where
      withGs l =
          concatMap (\s -> [s, 'g':s]) l

assertDefines :: Bool -> String -> [(String, String)]
assertDefines hunitBackwardsCompat prefix =
    concatMap fun allAsserts ++ [("assertFailure", expansion "assertFailure" "_")]
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

poorManAnalyzeTokens :: [LocToken] -> ModuleInfo
poorManAnalyzeTokens toks =
    -- show toks `trace`
    let revRes =
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
          (Reservedid, (_, "module")) : rest ->
              case rest of
                (Conid, (_, name)):rest2 ->
                    do setModName name
                       loop rest2
                (Qconid, (_, name)):rest2 ->
                    do setModName name
                       loop rest2
                _ -> loop rest
          (Varid, (loc, name)) : rest
              | isStartOfLine loc ->
                  case name of
                    't':'e':'s':'t':'_':shortName ->
                        do addTestDef shortName name (locToLocation loc)
                           loop rest
                    'p':'r':'o':'p':'_':shortName ->
                        do addPropDef shortName name (locToLocation loc)
                           loop rest
                    _ -> loop rest
              | otherwise -> loop rest
          (Special, (loc, "import_HTF_TESTS")) : rest ->
              case parseImport loc rest of
                Just (imp, rest2) ->
                    do addHtfImport imp
                       loop rest2
                Nothing -> loop rest
          (Reservedid, (loc, "import")) : rest ->
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
                    (Varid, (_, "qualified")):rest -> (True, rest)
                    _ -> (False, toks)
             (name, toks3) <-
                  case toks2 of
                    (Conid, (_, name)):rest -> return (name, rest)
                    (Qconid, (_, name)):rest -> return (name, rest)
                    _ -> fail "no import"
             let (mAlias, toks4) =
                   case toks3 of
                     (Varid, (_, "as")):(Conid, (_, alias)):rest -> (Just alias, rest)
                     _ -> (Nothing, toks3)
                 decl = ImportDecl { imp_moduleName = name
                                   , imp_qualified = qualified
                                   , imp_alias = mAlias
                                   , imp_loc = locToLocation loc }
             return (decl, toks4)
      locToLocation loc =
          makeLoc (l_file loc) (l_line loc)
      isStartOfLine loc =
          l_column loc == 1

cleanupTokens :: [PosToken] -> [PosToken]
cleanupTokens toks =
    -- Remove whitespace tokens, remove comments, but replace
    -- 'import {-@ HTF_TESTS @-}' with a single
    -- token Special with value "import_HTF_TESTS"
    case toks of
      (Whitespace, _):rest -> cleanupTokens rest
      (NestedComment, (loc, "{-@ HTF_TESTS @-}")) : rest ->
          (Special, (loc, "import_HTF_TESTS")) :
          cleanupTokens rest
      tok:rest -> tok : cleanupTokens rest
      [] -> []

-- char         -> ' (graphic<' | \> | space | escape<\&>) '
-- graphic      -> small | large | symbol | digit | special | : | " | '
-- escape       -> \ ( charesc | ascii | decimal | o octal | x hexadecimal )
-- charesc      -> a | b | f | n | r | t | v | \ | " | ' | &
-- ascii        -> ^cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
--               | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
--               | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN
--               | EM | SUB | ESC | FS | GS | RS | US | SP | DEL
-- cntrl        -> ascLarge | @ | [ | \ | ] | ^ | _
-- decimal      -> digit{digit}
-- octal        -> octit{octit}
-- hexadecimal  -> hexit{hexit}
-- octit        -> 0 | 1 | ... | 7
-- hexit        -> digit | A | ... | F | a | ... | f
-- special      -> ( | ) | , | ; | [ | ] | `| { | }
-- Purpose of cleanupInputString: filter out template Haskell quotes
cleanupInputString :: String -> String
cleanupInputString s =
    case s of
      c:'\'':'\'':x:rest
          | isSpace c && isUpper x ->         -- TH type quote
              c:x:cleanupInputString rest
      c:'\'':'\'':d:rest
          | not (isAlphaNum c) || not (isAlphaNum d)
              -> c:'\'':'x':'\'':d:rest
      '\'':rest ->
          case characterLitRest rest of
            Just (restLit, rest') ->
                '\'':restLit ++ cleanupInputString rest'
            Nothing ->
                '\'':cleanupInputString rest
      c:'\'':x:rest                -- TH name quote
          | isSpace c && isNothing (characterLitRest (x:rest)) && isLower x ->
              c:x:cleanupInputString rest
      c:rest
         | not (isSpace c) ->
             case span (== '\'') rest of
               (quotes, rest') -> c : quotes ++ cleanupInputString rest'
      c:rest -> c : cleanupInputString rest
      [] -> []
    where
      characterLitRest s = -- expects that before the s there is a '
          case s of
            '\\':'\'':'\'':rest -> Just ("\\''", rest)  -- '\''
            c:'\'':rest -> Just (c:"'", rest)           -- regular character lit
            '\\':rest ->
                case span (/= '\'') rest of
                  (esc,'\'':rest) ->
                      Just (('\\':esc) ++ "'", rest)
                  _ -> Just (s, "")      -- should not happen
            _ -> Nothing

cleanupInputStringTest =
    do flip mapM_ untouched $ \s ->
           let cleanedUp = cleanupInputString s
           in if s /= cleanedUp
              then assertFailure ("Cleanup of " ++ show s ++ " is wrong: " ++ show cleanedUp ++
                                  ", expected that input and output are the same")
              else return ()
       flip mapM_ touched $ \(input, output) ->
           let cleanedUp = cleanupInputString input
           in if output /= cleanedUp
              then assertFailure ("Cleanup of " ++ show input ++ " is wrong: " ++ show cleanedUp
                                  ++ ", expected " ++ show output)
              else return ()
    where
      untouched = [" '0'", " '\\''", " ' '", " '\o761'", " '\BEL'", " '\^@' ", "' '", "fixed' ' '"]
      touched = [(" 'foo abc", " foo abc"), (" ''T ", " T ")]

type LocToken =  (Token,(Loc,String))

data Loc
    = Loc
      { l_file :: FilePath
      , l_line :: Int
      , l_column :: Int
      }
    deriving (Eq, Show)

-- token stream should not contain whitespace
fixPositions :: FilePath -> [PosToken] -> [LocToken]
fixPositions originalFileName = loop Nothing
    where
      loop mPragma toks =
          case toks of
            [] -> []
            (Varsym, (pos, "#")) : (Varid, (_, "line")) : (IntLit, (_, lineNo)) : (StringLit,(_, fileName)) : rest
                | column pos == 1 ->
                    map (\(tt, (pos, x)) -> (tt, (fixPos Nothing pos, x))) (take 4 toks) ++
                    loop (Just (line pos, fileName, read lineNo)) rest
            (tt, (pos, x)) : rest ->
                (tt, (fixPos mPragma pos, x)) : loop mPragma rest
      fixPos mPragma pos =
          case mPragma of
            Nothing ->
                Loc { l_column = column pos
                    , l_file = originalFileName
                    , l_line = line pos
                    }
            Just (lineActivated, fileName, lineNo) ->
                let offset = line pos - lineActivated - 1
                in Loc { l_column = column pos
                       , l_file = fileName
                       , l_line = lineNo + offset
                       }

fixPositionsTest :: IO ()
fixPositionsTest =
    let toks = concatMap (\(f, i) -> f i)
                   (zip [tok, linePragma "bar" 10, tok, tok, linePragma "foo" 99, tok] [1..])
        fixedToks = fixPositions origFileName toks
        expectedToks = concat $
                       [tok' origFileName 1
                       ,linePragma' "bar" 10 2
                       ,tok' "bar" 10
                       ,tok' "bar" 11
                       ,linePragma' "foo" 99 5
                       ,tok' "foo" 99]
    in assertEqual (show expectedToks ++ "\n\n  /=  \n\n" ++ show toks) expectedToks fixedToks
    where
      origFileName = "spam"
      tok line = [(Varid, (Pos 0 line 1, "_"))]
      linePragma fname line lineHere =
          let pos = Pos 0 lineHere 1
          in [(Varsym, (pos, "#"))
             ,(Varid, (pos, "line"))
             ,(IntLit, (pos, show line))
             ,(StringLit, (pos, fname))]
      tok' fname line =
          let loc = Loc fname line 1
          in [(Varid, (loc, "_"))]
      linePragma' fname line lineHere =
          let loc = Loc origFileName lineHere 1
          in [(Varsym, (loc, "#"))
             ,(Varid, (loc, "line"))
             ,(IntLit, (loc, show line))
             ,(StringLit,(loc, fname))]

analyze :: FilePath -> String -> ModuleInfo
analyze originalFileName input =
    poorManAnalyzeTokens (fixPositions originalFileName (cleanupTokens (lexerPass0 (cleanupInputString input))))

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
              ,"test_blah test_foo = 1"
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
          let givenMi = analyze "<input>" src
          in if givenMi == mi
             then return ()
             else assertFailure ("Error in test " ++ show i ++
                                 ", expected:\n" ++ show mi ++
                                 "\nGiven:\n" ++ show givenMi ++
                                 "\nSrc:\n" ++ src)

transform :: Bool -> Bool -> FilePath -> String -> IO String
transform hunitBackwardsCompat debug originalFileName input =
    let info = analyze originalFileName fixedInput
    in preprocess info
    where
      preprocess :: ModuleInfo -> IO String
      preprocess info =
          do when debug $ hPutStrLn stderr ("Module info:\n" ++ show info)
             preProcessedInput <- runCpphs (cpphsOptions info) originalFileName
                                           fixedInput
             return $ preProcessedInput ++ "\n\n" ++ additionalCode info ++ "\n"
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
      cpphsOptions :: ModuleInfo -> CpphsOptions
      cpphsOptions info =
          defaultCpphsOptions { defines =
                                    defines defaultCpphsOptions ++
                                    assertDefines hunitBackwardsCompat (mi_htfPrefix info) ++
                                    nameDefines info
                              , boolopts = (boolopts defaultCpphsOptions) { lang = True } -- lex as haskell
                              }
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
    [("testAnalyze", testAnalyze)
    ,("fixPositionsTest", fixPositionsTest)
    ,("cleanupInputStringTest", cleanupInputStringTest)]
