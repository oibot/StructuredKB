module Paths_prioLinProbKB (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/tobi/Dropbox/Documents/Bachelorarbeit/Program/PrioLinProbKB/.cabal-sandbox/bin"
libdir     = "/Users/tobi/Dropbox/Documents/Bachelorarbeit/Program/PrioLinProbKB/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.3/prioLinProbKB-0.1.0.0-0iJuGFvTKdIHdlapIXZtnA"
datadir    = "/Users/tobi/Dropbox/Documents/Bachelorarbeit/Program/PrioLinProbKB/.cabal-sandbox/share/x86_64-osx-ghc-7.10.3/prioLinProbKB-0.1.0.0"
libexecdir = "/Users/tobi/Dropbox/Documents/Bachelorarbeit/Program/PrioLinProbKB/.cabal-sandbox/libexec"
sysconfdir = "/Users/tobi/Dropbox/Documents/Bachelorarbeit/Program/PrioLinProbKB/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "prioLinProbKB_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "prioLinProbKB_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "prioLinProbKB_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "prioLinProbKB_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "prioLinProbKB_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
