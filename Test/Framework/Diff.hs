{-# LANGUAGE CPP, ScopedTypeVariables #-}
--
-- Copyright (c) 2011, 2012   Stefan Wehr - http://www.stefanwehr.de
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

    DiffConfig(..), diffWithSensibleConfig, diff, main

) where

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif

import Control.Exception (catch, finally, IOException)
import qualified Data.List as List
import Data.Char
import qualified Data.Algorithm.Diff as D
import Data.Algorithm.DiffOutput
import Test.Framework.Colors

-- for testing
import System.IO
import System.Directory
import System.Exit
import System.Process
import System.Environment (getArgs)
import qualified Data.Text as T

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

data DiffConfig = DiffConfig {
    -- for single line diffs
      dc_fromFirst :: String -> ColorString
    , dc_fromSecond :: String -> ColorString
    , dc_fromBoth :: String -> ColorString
    , dc_sep :: ColorString
    , dc_skip :: String -> ColorString
    -- for multi-line diffs
    , dc_lineFromFirst :: String -> ColorString
    , dc_lineFromSecond :: String -> ColorString
    }

mkDefaultDiffConfig :: Color -> Color -> Color -> Char -> Char -> DiffConfig
mkDefaultDiffConfig c1 c2 c3 f s = DiffConfig {
      dc_fromFirst = \x -> colorize' c1 x (f : " " ++ x)
    , dc_fromSecond = \x -> colorize' c2 x (s : " " ++ x)
    , dc_fromBoth = \x -> noColor' x ("C " ++ x)
    , dc_skip = \x -> colorize' c3 ("..." ++ x ++ "...") ("<..." ++ x ++ "...>")
    , dc_sep = noColor' "" "\n"
    , dc_lineFromFirst = colorize c1
    , dc_lineFromSecond = colorize c2
    }

defaultDiffConfig :: DiffConfig
defaultDiffConfig = mkDefaultDiffConfig firstDiffColor secondDiffColor skipDiffColor 'F' 'S'

contextSize :: Int
contextSize = 10

singleLineDiff :: DiffConfig -> String -> String -> ColorString
singleLineDiff dc s1 s2
    | s1 == s2 = emptyColorString
    | otherwise =
        let groups = D.getGroupedDiff s1 s2
        in foldr (\(group, pos) string ->
                      (showDiffGroup pos group) +++
                      (if not (isLast pos) then dc_sep dc else emptyColorString) +++
                      string)
                 emptyColorString (addPositions groups)
    where
#if MIN_VERSION_Diff(0,2,0)
      showDiffGroup _ (D.First s) = dc_fromFirst dc s
      showDiffGroup _ (D.Second s) = dc_fromSecond dc s
      showDiffGroup pos (D.Both inBoth _) =
#else
      showDiffGroup _ (D.F, s) = dc_fromFirst dc s
      showDiffGroup _ (D.S, s) = dc_fromSecond dc s
      showDiffGroup pos (D.B, inBoth) =
#endif
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
                           replText = "skipped " ++ show n ++ " chars"
                       in if n <= length replText
                          then dc_skip dc ignored
                          else dc_skip dc replText
          in dc_fromBoth dc contextStart +++ middle +++ dc_fromBoth dc contextEnd
      addPositions [] = []
      addPositions (x:[]) = (x, FirstLast) : []
      addPositions (x:xs) = (x, First) : addPositions' xs
      addPositions' [] = []
      addPositions' (x:[]) = (x, Last) : []
      addPositions' (x:xs) = (x, Middle) : addPositions' xs

multiLineDiff :: DiffConfig -> String -> String -> IO ColorString
multiLineDiff cfg left right =
    withTempFiles $ \(fpLeft, hLeft) (fpRight, hRight) ->
        do write hLeft left
           write hRight right
           doDiff fpLeft fpRight
    where
      doDiff leftFile rightFile =
          (do (ecode, out, _err) <- readProcessWithExitCode "diff" [leftFile, rightFile] ""
              case ecode of
                ExitSuccess -> return (format out)
                ExitFailure 1 -> return (format out)
                ExitFailure _i -> return $ multiLineDiffHaskell left right)
             -- if we can't launch diff, use the Haskell code.
             -- We don't write the exception anywhere to not pollute test results.
            `catch` (\(_::IOException) -> return $ multiLineDiffHaskell left right)
      saveRemove fp =
          removeFile fp `catch` (\e -> hPutStrLn stderr (show (e::IOException)))
      withTempFiles action =
          do dir <- getTemporaryDirectory
             left@(fpLeft, _) <- openTempFile dir "HTF-diff-left.txt"
             (do right@(fpRight, _) <- openTempFile dir "HTF-diff-right.txt"
                 action left right `finally` saveRemove fpRight
              `finally` saveRemove fpLeft)
      write h s =
          do hPutStr h s
             hClose h
      format out = unlinesColorString $ map formatLine (lines out)
      formatLine l =
          case l of
            ('<' : _) -> fromFirst l
            ('>' : _) -> fromSecond l
            (c : _)
                 | isDigit c -> case List.span (\c -> c /= 'a' && c /= 'c' && c /= 'd') l of
                                  (left, c:right) -> fromFirst left +++
                                                     noColor [c] +++
                                                     fromSecond right
                                  (left, []) -> noColor left
                 | otherwise -> noColor l
            [] -> noColor l
          where
            fromFirst s = dc_fromFirst cfg s
            fromSecond s = dc_fromSecond cfg s

diff :: DiffConfig -> String -> String -> IO ColorString
diff cfg left right =
    case (lines left, lines right) of
      ([], []) -> return emptyColorString
      ([], [_]) -> return $ singleLineDiff cfg left right
      ([_], []) -> return $ singleLineDiff cfg left right
      ([_], [_]) -> return $ singleLineDiff cfg left right
      _ -> multiLineDiff cfg left right

diffWithSensibleConfig :: String -> String -> IO ColorString
diffWithSensibleConfig s1 s2 =
    diff defaultDiffConfig s1 s2

{-
Haskell diff, in case the diff tool is not present
-}
multiLineDiffHaskell :: String -> String -> ColorString
multiLineDiffHaskell left right =
    noColor $ ppDiff $ D.getGroupedDiff (lines left) (lines right) -- this code is now part of the Diff library (hence the >0.3 in Cabal)

main =
    do args <- getArgs
       (leftFp, rightFp) <-
           case args of
             [x] -> return (x, x)
             [x, y] -> return (x, y)
             _ -> fail ("USAGE: diff FILE1 FILE2")
       left <- readFile leftFp
       right <- readFile rightFp
       diff <- return $ multiLineDiffHaskell left right
       putStr $ T.unpack $ renderColorString diff True

-- Testcases:
--
-- < 12
-- vs.
-- > 1
-- > 2
