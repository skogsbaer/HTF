{-# LANGUAGE ScopedTypeVariables #-}

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

module Test.Framework.HaskellParser where

import Data.Maybe
import Control.Exception ( evaluate, catch, SomeException )
import Prelude hiding ( catch )

import qualified Language.Haskell.Exts.Parser as Parser
import qualified Language.Haskell.Exts.Syntax as Syn
import qualified Language.Haskell.Exts.Extension as Ext

import Test.Framework.Location

type Name = String

data Decl = Decl { decl_loc :: Location
                 , decl_name :: Name }

data ParseResult a = ParseOK a | ParseError Location String

data Module = Module { mod_name :: Name
                     , mod_imports :: [ImportDecl]
                     , mod_decls :: [Decl] }

data ImportDecl = ImportDecl { imp_moduleName :: Name
                             , imp_qualified :: Bool
                             , imp_alias :: Maybe Name }

parse :: FilePath -> String -> IO (ParseResult Module)
parse originalFileName input =
    do r <- (evaluate $ Parser.parseModuleWithMode parseMode fixedInput)
            `catch` (\(e::SomeException) ->
                         return $ Parser.ParseFailed unknownLoc (show e))
       case r of
         Parser.ParseFailed loc err -> return (ParseError (transformLoc loc) err)
         Parser.ParseOk m -> return $ ParseOK (transformModule m)
    where
      fixedInput :: String
      fixedInput = (input ++ "\n") {- the parser fails if the last line is a
                                      line comment not ending with \n -}
      parseMode :: Parser.ParseMode
      parseMode = Parser.defaultParseMode { Parser.parseFilename = originalFileName
                                          , Parser.extensions =
                                              Ext.glasgowExts ++
                                              [Ext.ExplicitForall]
                                   }
      unknownLoc :: Syn.SrcLoc
      unknownLoc = Syn.SrcLoc originalFileName 0 0
      transformModule (Syn.Module _ (Syn.ModuleName moduleName) _ _ _
                          imports decls) =
          Module moduleName (map transformImport imports)
                            (mapMaybe transformDecl decls)
      transformImport (Syn.ImportDecl loc (Syn.ModuleName s) qualified _ _ alias _) =
          let alias' = case alias of
                         Nothing -> Nothing
                         Just (Syn.ModuleName s) -> Just s
          in ImportDecl s qualified alias'
      transformDecl (Syn.PatBind loc (Syn.PVar name) _ _ _) =
          Just $ Decl (transformLoc loc) (transformName name)
      transformDecl (Syn.FunBind (Syn.Match loc name _ _ _ _ : _)) =
          Just $ Decl (transformLoc loc) (transformName name)
      transformDecl _ = Nothing
      transformLoc (Syn.SrcLoc f n _) = makeLoc f n
      transformName :: Syn.Name -> String
      transformName (Syn.Ident s) = s
      transformName (Syn.Symbol s) = s
