{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{- |
Utility module dealing with ANSI colors.
-}
--
-- Copyright (c) 2011-2022   Stefan Wehr - http://www.stefanwehr.de
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

module Test.Framework.Colors (

    Color(..), PrimColor(..), ColorString(..), PrimColorString(..)
  , firstDiffColor, secondDiffColor, skipDiffColor, diffColor
  , warningColor, testStartColor, testOkColor, pendingColor
  , emptyColorString, (+++), unlinesColorString, colorStringFind, ensureNewlineColorString
  , colorize, colorizeText, colorize', colorizeText'
  , noColor, noColorText, noColor', noColorText'
  , renderColorString, maxLength

) where

import qualified Data.Text as T
import Data.String
import Data.Maybe
import Control.Monad

firstDiffColor = Color Magenta False
secondDiffColor = Color Blue False
skipDiffColor = Color DarkGray False
diffColor = Color Brown False
warningColor = Color Red True
testStartColor = Color NoColor True
testOkColor = Color Green False
pendingColor = Color Cyan True

data Color = Color PrimColor Bool
           deriving (Eq, Show, Read)

data PrimColor = Black | Blue | Green | Cyan | Red | Magenta
               | Brown | Gray | DarkGray | LightBlue
               | LightGreen | LightCyan | LightRed | LightMagenta
               | Yellow | White | NoColor
             deriving (Eq, Show, Read)

startColor :: Color -> T.Text
startColor (Color c isBold) =
    (case c of
       Black -> "\ESC[0;30m"
       Blue -> "\ESC[0;34m"
       Green -> "\ESC[0;32m"
       Cyan -> "\ESC[0;36m"
       Red -> "\ESC[0;31m"
       Magenta -> "\ESC[0;35m"
       Brown -> "\ESC[0;33m"
       Gray -> "\ESC[0;37m"
       DarkGray -> "\ESC[1;30m"
       LightBlue -> "\ESC[1;34m"
       LightGreen -> "\ESC[1;32m"
       LightCyan -> "\ESC[1;36m"
       LightRed -> "\ESC[1;31m"
       LightMagenta -> "\ESC[1;35m"
       Yellow -> "\ESC[1;33m"
       White -> "\ESC[1;37m"
       NoColor -> "") `T.append`
    (if isBold then "\ESC[1m" else "")

reset :: T.Text
reset = "\ESC[0;0m"

data PrimColorString = PrimColorString Color T.Text (Maybe T.Text) {- no-color fallback -}
                    deriving (Eq, Show, Read)

newtype ColorString = ColorString { unColorString :: [PrimColorString] }
                    deriving (Eq, Show, Read)

instance IsString ColorString where
    fromString = noColor

emptyColorString :: ColorString
emptyColorString = noColor ""

maxLength :: ColorString -> Int
maxLength (ColorString prims) =
    let ml (PrimColorString _ t mt) =
            max (T.length t) (fromMaybe 0 (fmap T.length mt))
    in sum $ map ml prims

unlinesColorString :: [ColorString] -> ColorString
unlinesColorString l =
    concatColorString $
    map (\x -> appendPrimColorString x (PrimColorString (Color NoColor False) (T.pack "\n") Nothing)) l
    where
      appendPrimColorString (ColorString l) x =
          ColorString (l ++ [x])

concatColorString :: [ColorString] -> ColorString
concatColorString l =
    ColorString $ concatMap (\(ColorString l) -> l) l

colorStringFind :: (Char -> Bool) -> ColorString -> Bool -> Maybe Char
colorStringFind pred (ColorString l) c =
    let f = if c then pcolorStringFindColor else pcolorStringFindNoColor
    in msum (map f l)
    where
      pcolorStringFindColor (PrimColorString _ t _) = tfind t
      pcolorStringFindNoColor (PrimColorString _ t Nothing) = tfind t
      pcolorStringFindNoColor (PrimColorString _ _ (Just t)) = tfind t
      tfind t = T.find pred t

ensureNewlineColorString :: ColorString -> ColorString
ensureNewlineColorString cs@(ColorString l) =
    let (colors, noColors) = unzip $ map colorsAndNoColors (reverse l)
        nlColor = needsNl colors
        nlNoColor = needsNl noColors
    in if not nlColor && not nlNoColor
       then cs
       else ColorString (l ++
                         [PrimColorString (Color NoColor False) (mkNl nlColor)
                                              (Just (mkNl nlNoColor))])
    where
      mkNl True = "\n"
      mkNl False = ""
      colorsAndNoColors (PrimColorString _ t1 (Just t2)) = (t1, t2)
      colorsAndNoColors (PrimColorString _ t1 Nothing) = (t1, t1)
      needsNl [] = False
      needsNl (t:ts) =
          let t' = T.dropWhileEnd (\c -> c == ' ') t
          in if T.null t'
             then needsNl ts
             else T.last t' /= '\n'

colorize :: Color -> String -> ColorString
colorize c s = colorizeText c (T.pack s)

colorizeText :: Color -> T.Text -> ColorString
colorizeText !c !t = ColorString [PrimColorString c t Nothing]

colorize' :: Color -> String -> String -> ColorString
colorize' c s x = colorizeText' c (T.pack s) (T.pack x)

colorizeText' :: Color -> T.Text -> T.Text -> ColorString
colorizeText' !c !t !x = ColorString [PrimColorString c t (Just x)]

noColor :: String -> ColorString
noColor = colorize (Color NoColor False)

noColorText :: T.Text -> ColorString
noColorText = colorizeText (Color NoColor False)

noColor' :: String -> String -> ColorString
noColor' s1 s2 = colorize' (Color NoColor False) s1 s2

noColorText' :: T.Text -> T.Text -> ColorString
noColorText' t1 t2 = colorizeText' (Color NoColor False) t1 t2

infixr 5  +++

(+++) :: ColorString -> ColorString -> ColorString
cs1 +++ cs2 =
    case (cs1, cs2) of
      (ColorString [PrimColorString c1 t1 m1], ColorString (PrimColorString c2 t2 m2 : rest))
          | c1 == c2 ->
              let m3 = case (m1, m2) of
                         (Nothing, Nothing) -> Nothing
                         (Just x1, Just x2) -> Just (x1 `T.append` x2)
                         (Just x1, Nothing) -> Just (x1 `T.append` t2)
                         (Nothing, Just x2) -> Just (t1 `T.append` x2)
              in ColorString (PrimColorString c1 (t1 `T.append` t2) m3 : rest)
      (ColorString ps1, ColorString ps2) -> ColorString (ps1 ++ ps2)

renderColorString :: ColorString -> Bool -> T.Text
renderColorString (ColorString l) useColor =
    T.concat (map render l)
    where
      render = if useColor then renderColors else renderNoColors
      renderNoColors (PrimColorString _ _ (Just t)) = t
      renderNoColors (PrimColorString _ t Nothing) = t
      renderColors (PrimColorString c t _) =
          T.concat [startColor c, t, reset]
