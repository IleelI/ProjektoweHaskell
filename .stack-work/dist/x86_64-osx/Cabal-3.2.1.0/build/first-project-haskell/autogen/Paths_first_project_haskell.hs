{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_first_project_haskell (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/volcent/Documents/Studia/3 Sem/Projekty/JPR/2. Haskell Projektowe/first-project-haskell/.stack-work/install/x86_64-osx/585348e2b5ce98f16dd71f776e4eeb74610056f659ba9833c2060ba4d63f5b06/8.10.7/bin"
libdir     = "/Users/volcent/Documents/Studia/3 Sem/Projekty/JPR/2. Haskell Projektowe/first-project-haskell/.stack-work/install/x86_64-osx/585348e2b5ce98f16dd71f776e4eeb74610056f659ba9833c2060ba4d63f5b06/8.10.7/lib/x86_64-osx-ghc-8.10.7/first-project-haskell-0.1.0.0-D8oTSpGgW7A5bfvAnoxDpo-first-project-haskell"
dynlibdir  = "/Users/volcent/Documents/Studia/3 Sem/Projekty/JPR/2. Haskell Projektowe/first-project-haskell/.stack-work/install/x86_64-osx/585348e2b5ce98f16dd71f776e4eeb74610056f659ba9833c2060ba4d63f5b06/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/volcent/Documents/Studia/3 Sem/Projekty/JPR/2. Haskell Projektowe/first-project-haskell/.stack-work/install/x86_64-osx/585348e2b5ce98f16dd71f776e4eeb74610056f659ba9833c2060ba4d63f5b06/8.10.7/share/x86_64-osx-ghc-8.10.7/first-project-haskell-0.1.0.0"
libexecdir = "/Users/volcent/Documents/Studia/3 Sem/Projekty/JPR/2. Haskell Projektowe/first-project-haskell/.stack-work/install/x86_64-osx/585348e2b5ce98f16dd71f776e4eeb74610056f659ba9833c2060ba4d63f5b06/8.10.7/libexec/x86_64-osx-ghc-8.10.7/first-project-haskell-0.1.0.0"
sysconfdir = "/Users/volcent/Documents/Studia/3 Sem/Projekty/JPR/2. Haskell Projektowe/first-project-haskell/.stack-work/install/x86_64-osx/585348e2b5ce98f16dd71f776e4eeb74610056f659ba9833c2060ba4d63f5b06/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "first_project_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "first_project_haskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "first_project_haskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "first_project_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "first_project_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "first_project_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
