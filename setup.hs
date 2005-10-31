import IO
import System
import System.Directory
import Control.Exception
import System.Posix.Files

import Distribution.Simple
import Distribution.Simple.Install
import Distribution.PackageDescription
import Distribution.Simple.Utils
import Distribution.Simple.LocalBuildInfo
import Distribution.Compat.FilePath	

bins = ["scripts/HTFghc", "scripts/HTFghci"]
binMode = foldr1 unionFileModes [ownerModes, groupReadMode, groupExecuteMode,
                                 otherReadMode, otherExecuteMode]

myInstHook pkgDscr lbi verb b =
    handle (\e -> do hPutStrLn stderr (show e)) $
      do let binDir = mkBinDir pkgDscr lbi Nothing
             copy from = let to = binDir `joinFileName` (snd $ splitFileName from)
                             in do copyFileVerbose verb from to
                                   -- HACK: don't know why Cabal does not preserve
                                   -- permission
                                   setFileMode to binMode
         install pkgDscr lbi (Nothing, verb)
         -- HACK: don't know why Cabal does not preserve permission
         withExe pkgDscr
                 (\ (Executable e _ _) -> setFileMode (binDir `joinFileName` 
                                                      (e `joinFileExt` exeExtension))
                                            binMode)
         mapM_ copy bins

hooks = defaultUserHooks { instHook = myInstHook }

main = defaultMainWithHooks hooks
