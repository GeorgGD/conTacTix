{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_GLOSS (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,13,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/georggd/Projects/Haskell/GLOSS_1.13.0.1/.cabal-sandbox/bin"
libdir     = "/home/georggd/Projects/Haskell/GLOSS_1.13.0.1/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2/GLOSS-1.13.0.1-G6oHGrBFBE3TDGO36TCIq"
dynlibdir  = "/home/georggd/Projects/Haskell/GLOSS_1.13.0.1/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/georggd/Projects/Haskell/GLOSS_1.13.0.1/.cabal-sandbox/share/x86_64-linux-ghc-8.0.2/GLOSS-1.13.0.1"
libexecdir = "/home/georggd/Projects/Haskell/GLOSS_1.13.0.1/.cabal-sandbox/libexec"
sysconfdir = "/home/georggd/Projects/Haskell/GLOSS_1.13.0.1/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "GLOSS_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "GLOSS_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "GLOSS_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "GLOSS_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "GLOSS_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "GLOSS_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
