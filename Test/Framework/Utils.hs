module Test.Framework.Utils where

import System.Directory
import Data.Char 

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
             -> (FilePath -> [FilePath] -> IO Bool)  -- predicate that determines
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
                                 
