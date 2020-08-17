{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_W3c_EBNF (
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

bindir     = "/home/azevedo/.cabal/bin"
libdir     = "/home/azevedo/.cabal/lib/x86_64-linux-ghc-8.0.2/W3c-EBNF-0.1.0.0-BisIcDEuQORItKBaFDgOi7"
dynlibdir  = "/home/azevedo/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/azevedo/.cabal/share/x86_64-linux-ghc-8.0.2/W3c-EBNF-0.1.0.0"
libexecdir = "/home/azevedo/.cabal/libexec"
sysconfdir = "/home/azevedo/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "W3c_EBNF_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "W3c_EBNF_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "W3c_EBNF_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "W3c_EBNF_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "W3c_EBNF_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "W3c_EBNF_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
