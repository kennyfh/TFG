{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_TFG_kenflohua (
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

bindir     = "/home/kenny/Documents/TFG/TFG-kenflohua/.stack-work/install/x86_64-linux-tinfo6/b71121d4ff7b9bc93a2fac3d14b73f4accd401082c4d398511be85cf798f71b9/8.10.7/bin"
libdir     = "/home/kenny/Documents/TFG/TFG-kenflohua/.stack-work/install/x86_64-linux-tinfo6/b71121d4ff7b9bc93a2fac3d14b73f4accd401082c4d398511be85cf798f71b9/8.10.7/lib/x86_64-linux-ghc-8.10.7/TFG-kenflohua-0.1.0.0-7xyB7rttmgGHbxKOhnKOPE"
dynlibdir  = "/home/kenny/Documents/TFG/TFG-kenflohua/.stack-work/install/x86_64-linux-tinfo6/b71121d4ff7b9bc93a2fac3d14b73f4accd401082c4d398511be85cf798f71b9/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/kenny/Documents/TFG/TFG-kenflohua/.stack-work/install/x86_64-linux-tinfo6/b71121d4ff7b9bc93a2fac3d14b73f4accd401082c4d398511be85cf798f71b9/8.10.7/share/x86_64-linux-ghc-8.10.7/TFG-kenflohua-0.1.0.0"
libexecdir = "/home/kenny/Documents/TFG/TFG-kenflohua/.stack-work/install/x86_64-linux-tinfo6/b71121d4ff7b9bc93a2fac3d14b73f4accd401082c4d398511be85cf798f71b9/8.10.7/libexec/x86_64-linux-ghc-8.10.7/TFG-kenflohua-0.1.0.0"
sysconfdir = "/home/kenny/Documents/TFG/TFG-kenflohua/.stack-work/install/x86_64-linux-tinfo6/b71121d4ff7b9bc93a2fac3d14b73f4accd401082c4d398511be85cf798f71b9/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TFG_kenflohua_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TFG_kenflohua_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "TFG_kenflohua_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "TFG_kenflohua_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TFG_kenflohua_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TFG_kenflohua_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
