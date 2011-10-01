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

    DiffConfig(..), noColorsDiffConfig, coloredDiffConfig
  , defaultTerminalDiffConfig, defaultNoColorsDiffConfig
  , diffWithSensibleConfig, diff

) where

import Prelude hiding (catch)

import Control.Exception (catch, finally, IOException)
import qualified Data.List as List
import Data.Algorithm.Diff
import Data.Char (isDigit)
import Test.QuickCheck
import Text.PrettyPrint
import Debug.Trace (trace)

import Test.Framework.Colors
import Test.Framework.TestConfig

-- for testing
import System.IO
import System.Environment
import System.Directory
import System.IO.Unsafe (unsafePerformIO)
import System.Exit
import System.Process

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
          do (ecode, out, err) <- readProcessWithExitCode "diff" [leftFile, rightFile] ""
             case ecode of
               ExitSuccess -> return (format out)
               ExitFailure 1 -> return (format out)
               ExitFailure i ->
                   return ("'diff " ++ leftFile ++ " " ++ rightFile ++
                           "' failed with exit code " ++ show i ++
                           ": " ++ show err)
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
NOTE: This is *nearly* working. Originally, I wanted to implemented a pure Haskell
diff solution. At some point, however, I decided that it would be better to implement
a solution based on the diff tool. For now, I leave the code as it is.

type PrimDiff = [(DI, Char)]
type LineNo = Int

data Line = Line { line_number :: LineNo
                 , line_content :: String {- without trailing \n -}
                 }
            deriving (Show)

data LineRange = LineRange { lr_numbers :: (LineNo, LineNo)
                           , lr_contents :: [String] {- without trailing \n -}
                           }
            deriving (Show)

data Diff a = OnlyInLeft a LineNo
            | OnlyInRight a LineNo
            | InBoth a a
            deriving (Show)

instance Functor Diff where
    fmap f d = case d of
                 OnlyInLeft x n -> OnlyInLeft (f x) n
                 OnlyInRight x n -> OnlyInRight (f x) n
                 InBoth x y -> InBoth (f x) (f y)

multiLineDiff :: DiffConfig -> String -> String -> String
multiLineDiff cfg left right =
    let diff = getDiff left right :: PrimDiff
        diffByLine = List.unfoldr nextLine diff
        diffLines = let (_, _, l) = foldl diffLine (1, 1, []) diffByLine
                    in reverse l
        diffLineRanges = maximize diffLines
    in debug ("diff: " ++ show diff ++
              "\ndiffByLine: " ++ show diffByLine ++
              "\ndiffLines: " ++ show diffLines ++
              "\ndiffLineRanges: " ++ show diffLineRanges) $
       render $ prettyDiffs diffLineRanges
    where
      nextLine :: PrimDiff -> Maybe (PrimDiff, PrimDiff)
      nextLine [] = Nothing
      nextLine diff =
          -- FIXME: add support for \r\n
          case List.span (\d -> d /= (B, '\n')) diff of
            ([], _ : rest) -> nextLine rest
            (l, _ : rest) -> Just (l, rest)
            (l, []) -> Just (l, [])
      diffLine :: (Int, Int, [Diff Line]) -> PrimDiff -> (Int, Int, [Diff Line])
      diffLine (leftLineNo, rightLineNo, l) diff =
          case (\(x, y) -> (reverse x, reverse y)) $
               foldl (\(l, r) d -> case d of
                                     (F, c) -> (c : l, r)
                                     (S, c) -> (l, c : r)
                                     (B, c) -> (c : l, c : r))
                     ([], []) diff
          of ([], rightLine) -> (leftLineNo, rightLineNo + 1,
                                 OnlyInRight (Line rightLineNo rightLine) leftLineNo : l)
             (leftLine, []) -> (leftLineNo + 1, rightLineNo,
                                OnlyInLeft (Line leftLineNo leftLine) rightLineNo : l)
             (leftLine, rightLine)
                 | leftLine /= rightLine ->
                     (leftLineNo + 1, rightLineNo + 1,
                      InBoth (Line leftLineNo leftLine) (Line rightLineNo rightLine) : l)
                 | otherwise ->
                     (leftLineNo + 1, rightLineNo + 1, l)
      maximize :: [Diff Line] -> [Diff LineRange]
      maximize [] = []
      maximize (x : l) = maximize' (fmap (\a -> [a]) x) l
          where
            maximize' (OnlyInLeft xs rightLineNo) (OnlyInLeft y _ : rest) =
                maximize' (OnlyInLeft (y : xs) rightLineNo) rest
            maximize' (OnlyInRight xs leftLineNo) (OnlyInRight y _ : rest) =
                maximize' (OnlyInRight (y : xs) leftLineNo) rest
            maximize' (InBoth xs ys) (InBoth x y : rest) =
                maximize' (InBoth (x:xs) (y:ys)) rest
            maximize' acc rest = fmap mkLineRange acc : maximize rest
            mkLineRange :: [Line] -> LineRange
            mkLineRange [] = error ("multilineDiff: cannot convert an empty list of lines " ++
                                    "into a LineRange")
            mkLineRange r@(Line lastLineNo _ : _) =
                case reverse r of
                  l@(Line firstLineNo _ : _) -> LineRange (firstLineNo, lastLineNo)
                                                          (map line_content l)

prettyDiffs :: [Diff LineRange] -> Doc
prettyDiffs [] = empty
prettyDiffs (d : rest) = prettyDiff d $$ prettyDiffs rest
    where
      prettyDiff (OnlyInLeft inLeft lineNoRight) =
          prettyRange (lr_numbers inLeft) <> char 'd' <> int lineNoRight $$
          prettyLines '<' (lr_contents inLeft)
      prettyDiff (OnlyInRight inRight lineNoLeft) =
          int lineNoLeft <> char 'a' <> prettyRange (lr_numbers inRight) $$
          prettyLines '>' (lr_contents inRight)
      prettyDiff (InBoth inLeft inRight) =
          prettyRange (lr_numbers inLeft) <> char 'c' <> prettyRange (lr_numbers inRight) $$
          prettyLines '<' (lr_contents inLeft) $$
          text "---" $$
          prettyLines '>' (lr_contents inRight)
      prettyRange (start, end) =
          if start == end then int start else int start <> comma <> int end
      prettyLines start lines =
          vcat (map (\l -> char start <+> text l) lines)

--
-- Tests for diff
--

prop_diffOk :: DiffInput -> Bool
prop_diffOk inp =
    multiLineDiff cfg (di_left inp) (di_right inp) ==
    unsafePerformIO (runDiff (di_left inp) (di_right inp))
    where
      cfg = noColorsDiffConfig 'l' 'r'
      runDiff left right =
          do leftFile <- writeTemp left
             rightFile <- writeTemp right
             (ecode, out, err) <-
                 readProcessWithExitCode "diff" [leftFile, rightFile] ""
             -- putStrLn ("OUT:\n" ++ out)
             -- putStrLn ("ERR:\n" ++ err)
             -- putStrLn ("ECODE:\n" ++ show ecode)
             case ecode of
               ExitSuccess -> return out
               ExitFailure 1 -> return out
               ExitFailure i -> error ("'diff " ++ leftFile ++ " " ++ rightFile ++
                                       "' failed with exit code " ++ show i ++
                                       ": " ++ show err)
      writeTemp s =
          do dir <- getTemporaryDirectory
             (fp, h) <- openTempFile dir "HTF-diff.txt"
             hPutStr h s
             hClose h
             return fp

data DiffInput = DiffInput { di_left :: String, di_right :: String }
               deriving (Show)

leftDiffInput = unlines ["1", "2", "3", "4", "", "5", "6", "7"]

instance Arbitrary DiffInput where
    arbitrary =
        do let leftLines = lines leftDiffInput
           rightLinesLines <- mapM modifyLine (leftLines ++ [""])
           return $ DiffInput (unlines leftLines)
                              (unlines (concat rightLinesLines))
      where
        randomString =
            do c <- (elements (['a'..'z']))
               return [c]
        modifyLine :: String -> Gen [String]
        modifyLine str =
            do prefixLen <- frequency [(20-i, return i) | i <- [0..5]]
               prefix <- mapM (\_ -> randomString) [1..prefixLen]
               frequency [ (5, return (prefix ++ [str]))
                         , (3, return (prefix ++ ["XXX" ++ str]))
                         , (2, return prefix)
                         , (2, return [str])]

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
       diff <- return $ multiLineDiff defaultTerminalDiffConfig left right
       putStr diff

-- Testcases:
--
-- < 12
-- vs.
-- > 1
-- > 2
-}
