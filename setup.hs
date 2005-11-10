import IO
import System
import System.Directory
import Control.Exception
import System.Posix.Files
import Data.Version

import Distribution.Simple
import Distribution.Simple.Install
import Distribution.PackageDescription
import Distribution.Simple.Utils
import Distribution.Simple.LocalBuildInfo
import Distribution.Compat.FilePath	

bins = ["scripts/htf-ghc", "scripts/htf-ghci"]
binMode = foldr1 unionFileModes [ownerModes, groupReadMode, groupExecuteMode,
                                 otherReadMode, otherExecuteMode]
requiredVersion = Version [6,4,1] []
configurationFile = "Test/Framework/Configuration.hs"

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

myPostConfHook _ _ lbi = 
    do let comp = compilerFlavor . compiler $ lbi
           vers = compilerVersion . compiler $ lbi
       if comp /= GHC || vers < requiredVersion
          then do hPutStrLn stderr ("compiler not support: " ++ show comp ++ " " ++
                                    show vers ++ ". At least GHC " ++ 
                                    show requiredVersion ++ " is required")
                  return (ExitFailure 1)
          else do genConfiguration vers
                  return ExitSuccess

genConfiguration :: Version -> IO ()
genConfiguration vers = 
    let code = unlines
               [ "-- GENERATED AUTOMATICALLY, DO NOT EDIT!!"
               , "module Test.Framework.Configuration where"
               , "import Data.Version"
               , "ghcVersion = " ++ show vers
               ]
        in writeFile configurationFile code

hooks = defaultUserHooks { instHook = myInstHook, postConf = myPostConfHook }

main = defaultMainWithHooks hooks
