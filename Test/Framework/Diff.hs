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

module Test.Framework.Diff (

    DiffConfig(..), noColorsDiffConfig, coloredDiffConfig, showDiff
  , defaultTerminalDiffConfig, defaultNoColorsDiffConfig
  , diffWithSensibleConfig

) where

import Data.Algorithm.Diff

import Test.Framework.Colors
import Test.Framework.TestConfig

data Pos = First | Middle | Last | FirstLast
         deriving (Eq)

isLast :: Pos -> Bool
isLast Last = True
isLast FirstLast = True
isLast _ = False

isFirst :: Pos -> Bool
isFirst First = True
isFirst FirstLast = True
isFirst _ = False

isMiddle :: Pos -> Bool
isMiddle Middle = True
isMiddle _ = False

data DiffConfig = DiffConfig {
      dc_fromFirstPrefix :: String
    , dc_fromFirstSuffix :: String
    , dc_fromSecondPrefix :: String
    , dc_fromSecondSuffix :: String
    , dc_fromBothPrefix :: String
    , dc_fromBothSuffix :: String
    , dc_sep :: String
    , dc_skipPrefix :: String
    , dc_skipSuffix :: String
    }

noColorsDiffConfig :: Char -> Char -> DiffConfig
noColorsDiffConfig f s = DiffConfig {
      dc_fromFirstPrefix = f : " "
    , dc_fromFirstSuffix = ""
    , dc_fromSecondPrefix = s : " "
    , dc_fromSecondSuffix = ""
    , dc_fromBothPrefix = "C "
    , dc_fromBothSuffix = ""
    , dc_skipPrefix = "<"
    , dc_skipSuffix = ">"
    , dc_sep = "\n"
    }

coloredDiffConfig :: Color -> Color -> Color -> DiffConfig
coloredDiffConfig c1 c2 c3 = DiffConfig {
      dc_fromFirstPrefix = startColor c1
    , dc_fromFirstSuffix = reset
    , dc_fromSecondPrefix = startColor c2
    , dc_fromSecondSuffix = reset
    , dc_fromBothPrefix = ""
    , dc_fromBothSuffix = ""
    , dc_skipPrefix = startColor c3
    , dc_skipSuffix = reset
    , dc_sep = ""
    }


defaultTerminalDiffConfig :: DiffConfig
defaultTerminalDiffConfig = coloredDiffConfig firstDiffColor secondDiffColor skipDiffColor

defaultNoColorsDiffConfig :: DiffConfig
defaultNoColorsDiffConfig = noColorsDiffConfig 'F' 'S'

contextSize :: Int
contextSize = 10

showDiff :: DiffConfig -> String -> String -> String
showDiff dc s1 s2 =
    let groups = getGroupedDiff s1 s2
    in foldr (\(group, pos) string ->
                  (showDiffGroup pos group) ++
                  (if not (isLast pos) then dc_sep dc else "") ++
                  string)
             "" (addPositions groups)
    where
      showDiffGroup _ (F, s) = dc_fromFirstPrefix dc ++ s ++ dc_fromFirstSuffix dc
      showDiffGroup _ (S, s) = dc_fromSecondPrefix dc ++ s ++ dc_fromSecondSuffix dc
      showDiffGroup pos (B, inBoth) =
          let showStart = not $ isFirst pos
              showEnd = not $ isLast pos
              (contextStart, ignored, contextEnd) =
                  let (s, rest) = splitAt contextSize inBoth
                      (i, e) = splitAt (length rest - contextSize) rest
                      start = if showStart then s else ""
                      end = if showEnd then e else ""
                      ign = (if showStart then "" else s) ++ i ++
                            (if showEnd then "" else e)
                  in (start, ign, end)
              middle = let n = length ignored
                           replText = dc_skipPrefix dc ++ "skipped " ++ show n ++ " chars" ++ dc_skipSuffix dc
                       in if n <= length replText then ignored else replText
          in dc_fromBothPrefix dc ++ contextStart ++ middle ++ contextEnd ++ dc_fromBothSuffix dc
      addPositions [] = []
      addPositions (x:[]) = (x, FirstLast) : []
      addPositions (x:xs) = (x, First) : addPositions' xs
      addPositions' [] = []
      addPositions' (x:[]) = (x, Last) : []
      addPositions' (x:xs) = (x, Middle) : addPositions' xs


diffWithSensibleConfig :: String -> String -> IO String
diffWithSensibleConfig s1 s2 =
    do b <- useColors
       let dc = if b then defaultTerminalDiffConfig else defaultNoColorsDiffConfig
       return $ showDiff dc s1 s2
