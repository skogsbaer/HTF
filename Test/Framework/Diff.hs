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
            `catch` (\(_::IOException) -> return $ multiLineDiffHaskell cfg left right)            
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

type PrimDiff = [(D.DI, Char)]
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

--diffSameType :: Diff a-> Diff a -> Bool
--diffSameType (OnlyInLeft {}) (OnlyInLeft {})=True
--diffSameType (OnlyInRight {}) (OnlyInRight {})=True
--diffSameType (InBoth {}) (InBoth {})=True
--diffSameType _ _=False
--
--diffNextLine :: Diff a-> Diff a -> Bool
--diffNextLine (OnlyInLeft _ l1) (OnlyInLeft _ l2)=l2==l1+1
--diffNextLine (OnlyInRight _ l1) (OnlyInRight _ l2)=l2==l1+1
--diffNextLine _ _=False


instance Functor Diff where
    fmap f d = case d of
                 OnlyInLeft x n -> OnlyInLeft (f x) n
                 OnlyInRight x n -> OnlyInRight (f x) n
                 InBoth x y -> InBoth (f x) (f y)

multiLineDiffHaskell :: DiffConfig -> String -> String -> String
multiLineDiffHaskell cfg left right =
   let  gdiff=D.getGroupedDiff (lines left) (lines right)
        -- (_,_,_,rdiffLineRanges)=List.foldl' toLineRange (1,1,True,[]) (zip gdiff (tail $ gdiff ++ [(D.B,[])]))
        diffLineRanges = toLineRange 1 1 gdiff
        --diffLineRanges= reverse rdiffLineRanges
   in   debug ("\ngrouped : " ++ show gdiff++
              "\ndiffLineRanges: " ++ show diffLineRanges)
        (render $ prettyDiffs diffLineRanges) ++ "\n"
   where
          --toLineRange :: (Int,Int,Bool,[Diff LineRange]) -> ((D.DI,[String]),(D.DI,[String])) -> (Int,Int,Bool,[Diff LineRange])
          toLineRange :: Int -> Int -> [(D.DI,[String])] -> [Diff LineRange]
          toLineRange _ _ []=[]
          toLineRange leftLine rightLine ((D.B,ls):rs)=
                let lines=length ls
                in toLineRange (leftLine+lines) (rightLine+lines) rs
          toLineRange leftLine rightLine ((D.S,lsS):(D.F,lsF1):(D.B,[""]):(D.F,lsF2):rs) | ((last lsF2) == "") =
                let 
                    linesB=1
                    linesS=length lsS
                    linesF=length lsF1 + length lsF2
                    diff=InBoth (LineRange (leftLine,leftLine+linesF-1) (lsF1 ++ [""] ++ (init lsF2))) (LineRange (rightLine,rightLine+linesS-1) lsS)
                in diff : toLineRange (leftLine+linesF+linesB) (rightLine+linesS+linesB) rs      
          toLineRange leftLine rightLine ((D.S,lsS):(D.F,lsF):rs)=
                let linesS=length lsS
                    linesF=length lsF
                    diff=InBoth (LineRange (leftLine,leftLine+linesF-1) lsF) (LineRange (rightLine,rightLine+linesS-1) lsS)
                in diff : toLineRange (leftLine+linesF) (rightLine+linesS) rs
          toLineRange leftLine rightLine ((D.S,lsS):rs)=
                let linesS=length lsS
                    diff=OnlyInRight (LineRange (rightLine,rightLine+linesS-1) lsS) (leftLine-1)
                in diff : toLineRange leftLine (rightLine+linesS) rs      
          toLineRange leftLine rightLine  ((D.F,lsF):rs)=
                let linesF=length lsF
                    diff=OnlyInLeft (LineRange (leftLine,leftLine+linesF-1) lsF) (rightLine-1)
                in diff: toLineRange(leftLine+linesF) rightLine rs 
          toLineRange leftLine rightLine (_:rs)=toLineRange leftLine rightLine rs     

--multiLineDiffHaskell :: DiffConfig -> String -> String -> String
--multiLineDiffHaskell cfg left right =
--    let diff = D.getDiff left right :: PrimDiff
--        --diffByLine = List.unfoldr nextLine diff
--        --diffLines = let (_, _, l) = foldl diffLine (1, 1, []) diffByLine
--        --            in reverse l
--        -- (_,_,_,_,lineMap)=List.foldl' splitByLineMap (1,1,"","",Map.empty) diff
--        (leftLineNo, rightLineNo, leftLine, rightLine,splitted1)=List.foldl' splitByLines (1,1,Nothing,Nothing,[]) diff   
--        -- (cmp leftLineNo rightLineNo leftLine rightLine)++
--        splitted2=reverse (splitted1)
--        diffLineRanges = maximize  splitted2
--    in debug ("diff: " ++ show diff ++
--              "\ngrouped : " ++ (show $ D.getGroupedDiff (lines left) (lines right)) ++
--              "\nsplitted2: " ++ show splitted2++
--             -- "\nlineMap: " ++ show lineMap++
--              --"\ndiffByLine: " ++ show diffByLine ++
--              --"\ndiffLines: " ++ show diffLines ++
--              "\ndiffLineRanges: " ++ show diffLineRanges )
--       (render $ prettyDiffs diffLineRanges) ++ "\n"
--    where
----      splitByLineMap :: (Int,Int,String,String,Map.Map Int (String,String)) -> (D.DI, Char) -> (Int,Int,String,String,Map.Map Int (String,String))
----      splitByLineMap (leftLineNo, rightLineNo, leftLine, rightLine,m) (di,c)
----                | di==D.B =if c== '\n' 
----                        then (leftLineNo +1, rightLineNo+1, "", "",addLineMap leftLineNo rightLineNo leftLine rightLine m)
----                        else (leftLineNo,rightLineNo, leftLine++[c],rightLine++[c],m)
----                | di== D.F =if c== '\n' 
----                        then (leftLineNo +1, rightLineNo, "", "",addLineMap leftLineNo rightLineNo leftLine rightLine m)
----                        else (leftLineNo,rightLineNo, leftLine++[c],rightLine,m)      
----                | di== D.S =if c== '\n' 
----                        then (leftLineNo , rightLineNo+1, "", "",addLineMap leftLineNo rightLineNo leftLine rightLine m)
----                        else (leftLineNo,rightLineNo, leftLine,rightLine++[c],m) 
----      addLineMap :: Int -> Int -> String -> String -> Map.Map Int (String,String) -> Map.Map Int (String,String)
----      addLineMap leftLineNo rightLineNo leftLine rightLine m=
----                let
----                        m2=Map.insertWith (\(l1,r1) (l2,r2)->(l2++l1,r2++r1)) leftLineNo (leftLine,"") m
----                in      Map.insertWith (\(l1,r1) (l2,r2)->(l2++l1,r2++r1)) rightLineNo ("",rightLine) m2
--      splitByLines :: (Int,Int,Maybe String,Maybe String,[Diff Line]) -> (D.DI, Char) -> (Int,Int,Maybe String,Maybe String,[Diff Line])
--      splitByLines  (leftLineNo, rightLineNo, leftLine, rightLine,l) (di,c)
--                | di==D.B =if c== '\n' 
--                        then (leftLineNo +1, rightLineNo+1, Nothing, Nothing,cmp leftLineNo rightLineNo leftLine rightLine  ++ l)
--                        else (leftLineNo,rightLineNo,Just $ maybe [c] (++[c]) leftLine,Just $ maybe [c] (++[c]) rightLine,l)
--                | di== D.F =if c== '\n' 
--                        then (leftLineNo +1, rightLineNo, Just "", Nothing,cmp leftLineNo rightLineNo (maybe (Just "") Just leftLine) rightLine  ++ l)
--                        else (leftLineNo,rightLineNo, Just $ maybe [c] (++[c]) leftLine,rightLine,l)      
--                | di== D.S =if c== '\n' 
--                        then (leftLineNo , rightLineNo+1, Nothing, Just "",cmp leftLineNo rightLineNo leftLine rightLine  ++ l)
--                        else (leftLineNo,rightLineNo, leftLine,Just $ maybe [c] (++[c]) rightLine,l) 
--      cmp :: Int -> Int -> Maybe String -> Maybe String -> [Diff Line]
--      cmp leftLineNo rightLineNo  Nothing Nothing =[]
--      cmp leftLineNo rightLineNo  Nothing (Just rightLine) =[OnlyInRight (Line rightLineNo rightLine) (leftLineNo-1)]
--      cmp leftLineNo rightLineNo  (Just leftLine) Nothing = [OnlyInLeft (Line leftLineNo leftLine) rightLineNo]
--      cmp leftLineNo rightLineNo  (Just leftLine) (Just rightLine)
--                 | leftLine /= rightLine= if leftLineNo == rightLineNo 
--                        then
--                               [InBoth (Line leftLineNo leftLine) (Line rightLineNo rightLine)]
--                        else
--                               [ OnlyInRight (Line rightLineNo rightLine) leftLineNo , OnlyInLeft (Line leftLineNo leftLine) rightLineNo]
--                 | otherwise =[]   
----      nextLine :: PrimDiff -> Maybe (PrimDiff, PrimDiff)
----      nextLine [] = Nothing
----      nextLine diff =
----          -- FIXME: add support for \r\n
----          --case List.span (\d -> d /= (D.B, '\n')) diff of
----          case List.span (\d -> (snd d) /= '\n') diff of
----            ([], _ : rest) -> nextLine rest
----            (l, _ : rest) -> Just (l, rest)
----            (l, []) -> Just (l, [])
----      diffLine :: (Int, Int, [Diff Line]) -> PrimDiff -> (Int, Int, [Diff Line])
----      diffLine (leftLineNo, rightLineNo, l) diff =
----          case (\(x, y) -> (reverse x, reverse y)) $
----               foldl (\(l, r) d -> case d of
----                                     (D.F, c) -> (c : l, r)
----                                     (D.S, c) -> (l, c : r)
----                                     (D.B, c) -> (c : l, c : r))
----                     ([], []) diff
----          of ([], rightLine) -> (leftLineNo, rightLineNo + 1,
----                                 OnlyInRight (Line rightLineNo rightLine) leftLineNo : l)
----             (leftLine, []) -> (leftLineNo + 1, rightLineNo,
----                                OnlyInLeft (Line leftLineNo leftLine) rightLineNo : l)
----             (leftLine, rightLine)
----                 | leftLine /= rightLine -> -- if leftLineNo == rightLineNo 
----                       -- then
----                                (leftLineNo + 1, rightLineNo + 1,
----                                        InBoth (Line leftLineNo leftLine) (Line rightLineNo rightLine) : l)
----                     --   else
----                      --          (leftLineNo +1, rightLineNo + 1,
----                     --                   OnlyInRight (Line rightLineNo rightLine) leftLineNo : OnlyInLeft (Line leftLineNo leftLine) rightLineNo : l)
----                 | otherwise ->
----                     (leftLineNo + 1, rightLineNo + 1, l)
----      maximize :: [Diff Line] -> [Diff LineRange]
----      maximize [] = []
----      maximize (x : l) = maximize' (fmap (\a -> [a]) x) l
----          where
----            maximize' (OnlyInLeft xs rightLineNo) (OnlyInLeft y _ : rest) =
----                maximize' (OnlyInLeft (y : xs) rightLineNo) rest
----            maximize' (OnlyInRight xs leftLineNo) (OnlyInRight y _ : rest) =
----                maximize' (OnlyInRight (y : xs) leftLineNo) rest
----            maximize' (InBoth xs ys) (InBoth x y : rest) =
----                maximize' (InBoth (x:xs) (y:ys)) rest
----            maximize' acc rest = fmap mkLineRange acc : maximize rest
----            mkLineRange :: [Line] -> LineRange
----            mkLineRange [] = error ("multilineDiff: cannot convert an empty list of lines " ++
----                                    "into a LineRange")
----            mkLineRange r@(Line lastLineNo _ : _) =
----                case reverse r of
----                  l@(Line firstLineNo _ : _) -> LineRange (firstLineNo, lastLineNo)
----                                                          (map line_content l)
--      maximize :: [Diff Line] -> [Diff LineRange]
--      maximize [] = []
--      maximize l = -- maximize' (fmap (\a -> [a]) x) l
--          let 
--                (o1,s1,t1)=List.foldl' doLeft ([],OnlyInLeft [] 0,[]) l
--                rAfterLeft=[s1]++(map (fmap (\x->[x])) o1)++t1
--                (o2,s2,t2)=List.foldr doRight ([],OnlyInRight [] 0,[]) rAfterLeft
--                r=reverse $ map (fmap mkLineRange) (o2++(fmap reverse s2):t2)
--                mRights=foldr getRights Map.empty r
--                mLefts=foldr getLefts mRights r
--                mReconciled=mapMaybe (reconcileInBoth mLefts) r
--          in mReconciled      
--          where
--            doLeft :: ([Diff Line],Diff [Line],[Diff [Line]]) -> Diff Line ->  ([Diff Line],Diff [Line],[Diff [Line]])
--            doLeft (others,same,total) il@(OnlyInLeft l n)=case same of
--                        OnlyInLeft [] _->(others,OnlyInLeft [l] n,total)
--                        OnlyInLeft (x:xs) l2->if (line_number x)==(line_number l)-1
--                                then (others,OnlyInLeft (l:x:xs) n,total)
--                                else ([],OnlyInLeft [l] n,[same]++(map (fmap (\x->[x])) others)++total)
--            doLeft (others,s@(OnlyInLeft [] _),total) l=(others,s,((fmap (\x->[x]) l):total))
--            doLeft (others,same,total) l=(l:others,same,total)
--            doRight :: Diff [Line] ->([Diff [Line]],Diff [Line],[Diff [Line]]) ->  ([Diff [Line]],Diff [Line],[Diff [Line]])
--            doRight (OnlyInRight l n) (others,same,total) =case same of
--                        OnlyInRight [] _->(others,OnlyInRight l n,total)
--                        OnlyInRight pr l2->if (line_number $ last pr)==(line_number $ head l)-1
--                                then (others,OnlyInRight (pr++l) l2,total)
--                                else ([],OnlyInRight l n,others++(fmap reverse same):total)
--            doRight l (others,s@(OnlyInRight [] _),total)=(others,s,l:total)
--            doRight l (others,same,total)=(l:others,same,total)
--            getRights :: Diff LineRange -> Map.Map Int (Diff LineRange) -> Map.Map Int (Diff LineRange)
--            getRights d@(OnlyInRight _ leftLine) m=Map.insert (leftLine+1) d m
--            getRights _ m=m
--            getLefts :: Diff LineRange -> Map.Map Int (Diff LineRange) -> Map.Map Int (Diff LineRange)
--            getLefts d@(OnlyInLeft lrL leftLine) m=case Map.lookup (fst $ lr_numbers lrL) m of
--                Just (OnlyInRight lrR _)->Map.insert (fst $ lr_numbers lrL) (InBoth lrL lrR) m
--                _ -> m
--            getLefts _ m=m
--            reconcileInBoth :: Map.Map Int (Diff LineRange) -> Diff LineRange -> Maybe (Diff LineRange)
--            reconcileInBoth m dR@(OnlyInRight _ leftLine)=case Map.lookup (leftLine+1) m of
--                 Just dB@(InBoth _ _)->Just dB
--                 _->Just dR
--            reconcileInBoth m dL@(OnlyInLeft lrL _)=case Map.lookup (fst $ lr_numbers lrL) m of
--                 Just (InBoth _ _)->Nothing
--                 _->Just dL
--            reconcileInBoth _ d=Just d          
----            maximize' (OnlyInLeft xs rightLineNo) (OnlyInLeft y _ : rest) =
----                maximize' (OnlyInLeft (y : xs) rightLineNo) rest
----            maximize' (OnlyInRight xs leftLineNo) (OnlyInRight y _ : rest) =
----                maximize' (OnlyInRight (y : xs) leftLineNo) rest
----            maximize' (InBoth xs ys) (InBoth x y : rest) =
----                maximize' (InBoth (x:xs) (y:ys)) rest
----            maximize' acc rest = fmap mkLineRange acc : maximize rest
--            mkLineRange :: [Line] -> LineRange
--            mkLineRange [] = error ("multilineDiff: cannot convert an empty list of lines " ++
--                                    "into a LineRange")
--            mkLineRange r@(Line lastLineNo _ : _) =
--                case reverse r of
--                  l@(Line firstLineNo _ : _)  -> LineRange (firstLineNo, lastLineNo)
--                                                          (map line_content l)
----                        | firstLineNo<=lastLineNo-> LineRange (firstLineNo, lastLineNo)
----                                                          (reverse $ map line_content l)
----                        | otherwise-> LineRange (lastLineNo, firstLineNo)
----                                                          (map line_content l)

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
    multiLineDiffHaskell cfg (di_left inp) (di_right inp) ==
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

test_diffOk = do
        runTest "tests/jp/wikiOriginal.txt" "tests/jp/wikiNew.txt"
        runTest "tests/jp/HTF-diff6782.txt" "tests/jp/HTF-diff6783.txt"
        runTest "tests/jp/HTF-diff3068.txt" "tests/jp/HTF-diff3069.txt"
        runTest "tests/jp/HTF-diff6472.txt" "tests/jp/HTF-diff6473.txt"
        where
                cfg = noColorsDiffConfig 'l' 'r'
                runTest f1 f2=do
                    s1<-readFile f1
                    s2<-readFile f2
                    (_, out, _) <-readProcessWithExitCode "diff" [f1, f2] ""
                    let haskellDiff=multiLineDiffHaskell cfg s1 s2
                    assertEqual "" out haskellDiff

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
       diff <- return $ multiLineDiffHaskell defaultTerminalDiffConfig left right
       putStr diff

-- Testcases:
--
-- < 12
-- vs.
-- > 1
-- > 2

