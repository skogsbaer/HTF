--
-- Copyright (c) 2011   Stefan Wehr - http://www.stefanwehr.de
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

    Color(..), PrimColor(..), startColor, withColor, colorize
  , reset
  , firstDiffColor, secondDiffColor, skipDiffColor
  , warningColor, testStartColor, testOkColor, pendingColor

) where

import Test.Framework.TestConfig

-- REVERSE            = "\033[2m"

firstDiffColor = Color Magenta False
secondDiffColor = Color Blue False
skipDiffColor = Color DarkGray False
warningColor = Color Red True
testStartColor = Color NoColor True
testOkColor = Color Green False
pendingColor = Color Cyan True

data Color = Color PrimColor Bool

data PrimColor = Black | Blue | Green | Cyan | Red | Magenta
               | Brown | Gray | DarkGray | LightBlue
               | LightGreen | LightCyan | LightRed | LightMagenta
               | Yellow | White | NoColor
             deriving (Eq, Show)

startColor :: Color -> String
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
       NoColor -> "") ++
    (if isBold then "\ESC[1m" else "")

reset :: String
reset = "\ESC[0;0m"

withColor :: Color -> String -> String
withColor c s = startColor c ++ s ++ reset

colorize :: Color -> String -> IO String
colorize c s =
    do b <- useColors
       return $ if b then withColor c s else s
