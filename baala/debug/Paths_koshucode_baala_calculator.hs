{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_koshucode_baala_calculator (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
version = Version [0,0,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "sandbox/bin"
libdir     = "sandbox/lib"
datadir    = "sandbox/share"
libexecdir = "sandbox/libexec"
sysconfdir = "sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "koshucode_baala_calculator_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "koshucode_baala_calculator_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "koshucode_baala_calculator_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "koshucode_baala_calculator_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "koshucode_baala_calculator_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
