{- |
Internal module for pretty-printing showable Haskell values.
-}
--
-- Copyright (c) 2009-2022 Stefan Wehr - http://www.stefanwehr.de
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
module Test.Framework.PrettyHaskell (

    prettyHaskell, prettyHaskell', prettyHaskellTests

) where

import qualified Data.List as List
import Language.Haskell.Parser
import Language.Haskell.Pretty
import Test.HUnit
import Test.Framework.Utils

prettyHaskell :: Show a => a -> String
prettyHaskell x =
    case prettyHaskell' x of
      Just s -> s
      Nothing -> "FALLBACK: " ++ show x

prettyHaskell' :: Show a => a -> Maybe String
prettyHaskell' x =
    fmap (postProcess (show x)) (prettyHaskell'' x )

prettyHaskell'' :: Show a => a -> Maybe String
prettyHaskell'' x =
    let str = show x
        code = "module M where TOP = " ++ str
    in case parseModule code of
         ParseOk x -> Just (prettyPrint x)
         ParseFailed _ _ -> Nothing

postProcess :: String -> String -> String
postProcess fallback s =
    case dropWhile (\l -> not ('=' `elem` l)) (lines s) of
      [] -> fallback
      (l:ls) ->
          case List.span (/= '=') l of
            (prefix, '=':' ':suffix) ->
                let indentLen = length prefix + 2
                in strip $ unlines (suffix : (map (drop indentLen) ls))
            _ -> fallback

prettyHaskellTests =
    [("testPrettyHaskell", testPrettyHaskell)]

data MySuperHero
    = MySuperHero
      { msh_age :: Int
      , msh_name :: String
      , msh_address :: String
      , msh_fun :: Int
      }
    deriving (Show)

data MySuperSuperHero
    = MySuperSuperHero
      { mssh_isHere :: Bool
      , mssh_hero :: MySuperHero
      }
    deriving (Show)

testPrettyHaskell =
    do assertPretty "Just 1" (Just 1)
       let hero =
               MySuperHero
               { msh_age = 35
               , msh_name = "FOO"
               , msh_address = "address"
               , msh_fun = 1
               }
       assertPretty
         ("MySuperHero{msh_age = 35, msh_name = \"FOO\",\n" ++
          "            msh_address = \"address\", msh_fun = 1}")
         hero
       assertPretty
          ("MySuperSuperHero{mssh_isHere = True,\n" ++
           "                 mssh_hero =\n" ++
           "                   MySuperHero{msh_age = 35, msh_name = \"FOO\",\n" ++
           "                               msh_address = \"address\", msh_fun = 1}}")
          (MySuperSuperHero { mssh_isHere = True, mssh_hero = hero })
    where
      assertPretty s x =
          assertEqual (s ++ " /=\n" ++ prettyHaskell x)
                      s (prettyHaskell x)
