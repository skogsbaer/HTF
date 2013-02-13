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

    DiffConfig(..), noColorsDiffConfig, coloredDiffConfig
  , defaultTerminalDiffConfig, defaultNoColorsDiffConfig
  , diffWithSensibleConfig, diff

) where

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif

import Control.Exception (catch, finally, IOException)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Char
import qualified Data.Algorithm.Diff as D
import Data.Algorithm.DiffOutput
import Test.Framework.Colors
import Test.Framework.Pretty

-- for testing
import System.IO
import System.Directory
import System.Exit
import System.Process
import Test.QuickCheck 
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)
import System.Environment (getArgs)
import Data.Maybe (mapMaybe, catMaybes)

import Test.HUnit

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
    -- for single line diffs
      dc_fromFirstPrefix :: String
    , dc_fromFirstSuffix :: String
    , dc_fromSecondPrefix :: String
    , dc_fromSecondSuffix :: String
    , dc_fromBothPrefix :: String
    , dc_fromBothSuffix :: String
    , dc_sep :: String
    , dc_skipPrefix :: String
    , dc_skipSuffix :: String
    -- for multi-line diffs
    , dc_lineFromFirstPrefix :: String
    , dc_lineFromSecondPrefix :: String
    , dc_lineFromFirstSuffix :: String
    , dc_lineFromSecondSuffix :: String
    }

noColorsDiffConfig :: Char -> Char -> DiffConfig
noColorsDiffConfig f s = DiffConfig {
      dc_fromFirstPrefix = f : " "
    , dc_fromFirstSuffix = ""
    , dc_fromSecondPrefix = s : " "
    , dc_fromSecondSuffix = ""
    , dc_fromBothPrefix = "C "
    , dc_fromBothSuffix = ""
    , dc_skipPrefix = "<..."
    , dc_skipSuffix = "...>"
    , dc_sep = "\n"
    , dc_lineFromFirstPrefix = ""
    , dc_lineFromSecondPrefix = ""
    , dc_lineFromFirstSuffix = ""
    , dc_lineFromSecondSuffix = ""
    }

coloredDiffConfig :: Color -> Color -> Color -> DiffConfig
coloredDiffConfig c1 c2 c3 = DiffConfig {
      dc_fromFirstPrefix = startColor c1
    , dc_fromFirstSuffix = reset
    , dc_fromSecondPrefix = startColor c2
    , dc_fromSecondSuffix = reset
    , dc_fromBothPrefix = ""
    , dc_fromBothSuffix = ""
    , dc_skipPrefix = startColor c3 ++ "..."
    , dc_skipSuffix = "..." ++ reset
    , dc_sep = ""
    , dc_lineFromFirstPrefix = startColor c1
    , dc_lineFromSecondPrefix = startColor c2
    , dc_lineFromFirstSuffix = reset
    , dc_lineFromSecondSuffix = reset
    }

defaultTerminalDiffConfig :: DiffConfig
defaultTerminalDiffConfig = coloredDiffConfig firstDiffColor secondDiffColor skipDiffColor

defaultNoColorsDiffConfig :: DiffConfig
defaultNoColorsDiffConfig = noColorsDiffConfig 'F' 'S'

contextSize :: Int
contextSize = 10

singleLineDiff :: DiffConfig -> String -> String -> String
singleLineDiff dc s1 s2
    | s1 == s2 = ""
    | otherwise =
        let groups = D.getGroupedDiff s1 s2
        in foldr (\(group, pos) string ->
                      (showDiffGroup pos group) ++
                      (if not (isLast pos) then dc_sep dc else "") ++
                      string)
                 "" (addPositions groups)
    where
#if MIN_VERSION_Diff(0,2,0)
      showDiffGroup _ (D.First s) = dc_fromFirstPrefix dc ++ s ++ dc_fromFirstSuffix dc
      showDiffGroup _ (D.Second s) = dc_fromSecondPrefix dc ++ s ++ dc_fromSecondSuffix dc
      showDiffGroup pos (D.Both inBoth _) =
#else
      showDiffGroup _ (D.F, s) = dc_fromFirstPrefix dc ++ s ++ dc_fromFirstSuffix dc
      showDiffGroup _ (D.S, s) = dc_fromSecondPrefix dc ++ s ++ dc_fromSecondSuffix dc
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
                           replText = dc_skipPrefix dc ++ "skipped " ++ show n ++ " chars" ++
                                      dc_skipSuffix dc
                       in if n <= length replText then ignored else replText
          in dc_fromBothPrefix dc ++ contextStart ++ middle ++ contextEnd ++
             dc_fromBothSuffix dc
      addPositions [] = []
      addPositions (x:[]) = (x, FirstLast) : []
      addPositions (x:xs) = (x, First) : addPositions' xs
      addPositions' [] = []
      addPositions' (x:[]) = (x, Last) : []
      addPositions' (x:xs) = (x, Middle) : addPositions' xs

multiLineDiff :: DiffConfig -> String -> String -> IO String
multiLineDiff cfg left right =
    withTempFiles $ \(fpLeft, hLeft) (fpRight, hRight) ->
        do write hLeft left
           write hRight right
           doDiff fpLeft fpRight
    where
      doDiff leftFile rightFile =
          (do (ecode, out, err) <- readProcessWithExitCode "diff" [leftFile, rightFile] ""
              case ecode of
                ExitSuccess -> return (format out)
                ExitFailure 1 -> return (format out)
                ExitFailure i ->
                   return ("'diff " ++ leftFile ++ " " ++ rightFile ++
                           "' failed with exit code " ++ show i ++
                           ": " ++ show err)) 
             -- if we can't launch diff, use the Haskell code. We don't write the exception anywhere to not pollute test results.              
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
      format out = unlines $ map formatLine (lines out)
      formatLine l =
          case l of
            ('<' : _) -> fromFirst l
            ('>' : _) -> fromSecond l
            (c : _)
                 | isDigit c -> case List.span (\c -> c /= 'a' && c /= 'c' && c /= 'd') l of
                                  (left, c:right) -> fromFirst left ++ [c] ++ fromSecond right
                                  (left, []) -> left
                 | otherwise -> l
          where
            fromFirst s = dc_fromFirstPrefix cfg ++ s ++ dc_fromFirstSuffix cfg
            fromSecond s = dc_fromSecondPrefix cfg ++ s ++ dc_fromSecondSuffix cfg

diff :: DiffConfig -> String -> String -> IO String
diff cfg left right =
    case (lines left, lines right) of
      ([], []) -> return ""
      ([], [_]) -> return $ singleLineDiff cfg left right
      ([_], []) -> return $ singleLineDiff cfg left right
      ([_], [_]) -> return $ singleLineDiff cfg left right
      _ -> multiLineDiff cfg left right

diffWithSensibleConfig :: String -> String -> IO String
diffWithSensibleConfig s1 s2 =
    do b <- useColors
       let dc = if b then defaultTerminalDiffConfig else defaultNoColorsDiffConfig
       diff dc s1 s2

{-
Haskell diff, in case the diff tool is not present
-}
multiLineDiffHaskell :: String -> String -> String
multiLineDiffHaskell left right =ppDiff $ D.getGroupedDiff (lines left) (lines right) -- this code is now part of the Diff library (hence the >0.3 in Cabal)
 

debug = trace
-- debug _ x = x

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
       putStr diff

-- Testcases:
--
-- < 12
-- vs.
-- > 1
-- > 2

