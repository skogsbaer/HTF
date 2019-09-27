{-# LANGUAGE PatternGuards #-}
--
-- Copyright (c) 2005, 2012   Stefan Wehr - http://www.stefanwehr.de
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

module Test.Framework.Utils where

import System.Directory
import Data.Char
import System.Time hiding (diffClockTimes)
import System.Random
import Data.Array.IO
import Control.Monad

infixr 6 </>

(</>) :: FilePath -> FilePath -> FilePath
[] </> b = b
a  </> b = a ++ "/" ++ b

basename :: FilePath -> FilePath
basename p = reverse $ takeWhile (/= '/') $ reverse p

dirname :: FilePath -> FilePath
dirname p  =
    case reverse $ dropWhile (/= '/') $ reverse p of
        [] -> "."
        p' -> p'

startswith :: String -> String -> Bool
startswith s pref =
    let n = length pref
        in take n s == pref

endswith :: String -> String -> Bool
endswith s suf =
    let n = length s - length suf
        in drop n s == suf

dropPrefix :: String -> String -> String
dropPrefix s pref =
    if startswith s pref
       then drop (length pref) s
       else s

dropSuffix :: FilePath -> FilePath
dropSuffix f = reverse . tail . dropWhile (/= '.') $ reverse f

replaceSuffix :: FilePath -> String -> FilePath
replaceSuffix f suf = dropSuffix f ++ suf

-- > dropSpace "   abc  " ===> "abc"
dropSpace :: [Char] -> [Char]
dropSpace = let f = reverse . dropWhile isSpace in f . f

data DirectoryEntryType = File | Directory | Other
                        deriving (Eq, Show)

directoryEntryType :: FilePath -> IO DirectoryEntryType
directoryEntryType fp =
    do b <- doesFileExist fp
       if b then return File else do b <- doesDirectoryExist fp
                                     return $ if b then Directory else Other

collectFiles :: FilePath                -- the directory to start from
             -> String                  -- suffix of the file names to collect
             -> (FilePath -> [FilePath] -> IO Bool)
               -- predicate that determines
               -- whether files below a certain
               -- directory should be pruned.
               -- The first argument is the
               -- name of the directory, the
               -- second the entries of the
               -- directory
             -> IO [FilePath]
collectFiles root suf prune =
    do entries <- getDirectoryContents root
       b <- prune root entries
       if b then return []
          else do all <- mapM (collect root) entries
                  return $ concat all
    where collect root f | f == "." || f == ".." = return []
                         | otherwise =
              do t <- directoryEntryType (root </> f)
                 case t of
                   Directory -> collectFiles (root </> f) suf prune
                   File | f `endswith` suf -> return [root </> f]
                   _ -> return []

maybeFile :: FilePath -> IO (Maybe FilePath)
maybeFile f =
    do b <- doesFileExist f
       return $ if b then Just f else Nothing

-- monadic version of mapAccumL
mapAccumLM :: Monad m
           => (acc -> x -> m (acc, y)) -- Function of elt of input list
                                       -- and accumulator, returning new
                                       -- accumulator and elt of result list
          -> acc            -- Initial accumulator
          -> [x]            -- Input list
          -> m (acc, [y])   -- Final accumulator and result list
mapAccumLM _ s []        = return (s, [])
mapAccumLM f s (x:xs)    = do (s', y ) <- f s x
                              (s'',ys) <- mapAccumLM f s' xs
                              return (s'',y:ys)

readM :: (MonadFail m, Read a) => String -> m a
readM s | [x] <- parse = return x
        | otherwise    = fail $ "Failed parse: " ++ show s
    where
      parse = [x | (x, []) <- reads s]

ensureNewline :: String -> String
ensureNewline s =
    s ++ case dropWhile (== ' ') (reverse s) of
           '\n':_ -> ""
           _ | null s -> ""
             | otherwise -> "\n"

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Measures execution time of the given IO action in milliseconds
measure :: IO a -> IO (a, Int)
measure ma =
    do t0 <- getClockTime
       a <- ma
       t1 <- a `seq` getClockTime
       let diffMicro = t1 `diffClockTimes` t0
       return (a, fromInteger (diffMicro `div` 1000))

diffClockTimes :: ClockTime -> ClockTime -> Integer
diffClockTimes (TOD s1 p1) (TOD s0 p0) =
    (picoseconds p1 + seconds s1) -
    (picoseconds p0 + seconds s0)
    where
      -- bring all into microseconds
      picoseconds i = i `div` (1000 * 1000)
      seconds i = i * 1000000

-- | Randomly shuffle a list
--   /O(N)/
shuffleIO :: [a] -> IO [a]
shuffleIO xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
