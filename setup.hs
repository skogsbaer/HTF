#!/usr/bin/env runhaskell

import System.IO
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

import DarcsPatchInfo

--
-- Version information
--

version = "0.1"

-- set `isPre' to `False' for official releases
isPre = True


--
-- Installation of scripts
--
 
bins = ["scripts/htf-ghc", "scripts/htf-ghci"]
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


--
-- Generate GHC version file
--

requiredVersion = Version [6,4,1] []
configurationFile = "Test/Framework/Configuration.hs"

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


--
-- Generate .cabal file
--

genCabal = 
    let from = "HTF.cabal.template"
        to = "HTF.cabal"
    in do s <- readFile from
          vers <- getVersion
          let s' = unlines $ "-- GENERATED AUTOMATICALLY, DO NOT EDIT!!" : 
                             (map (setVersion vers) $ lines s)
          writeFile to s'
    where getVersion = if not isPre then return version else
              do b <- isUnderDarcsControl
                 if not b then return (version ++ "-pre") else
                    do dpi <- getDarcsPatchInfo
                       return (version ++ "-pre-" ++ formatDarcsPatchInfo dpi)
          setVersion vers ('V':'e':'r':'s':'i':'o':'n':':':_) =
              "Version: " ++ vers
          setVersion _ s = s
              

hooks = defaultUserHooks { instHook = myInstHook, postConf = myPostConfHook }

main = 
    do genCabal
       defaultMainWithHooks hooks
