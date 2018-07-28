{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_financial_news_scraper (
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

bindir     = "/Users/v0d1ch/.cabal/bin"
libdir     = "/Users/v0d1ch/.cabal/lib/x86_64-osx-ghc-8.2.2/financial-news-scraper-0.1.0.0-inplace"
dynlibdir  = "/Users/v0d1ch/.cabal/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/v0d1ch/.cabal/share/x86_64-osx-ghc-8.2.2/financial-news-scraper-0.1.0.0"
libexecdir = "/Users/v0d1ch/.cabal/libexec/x86_64-osx-ghc-8.2.2/financial-news-scraper-0.1.0.0"
sysconfdir = "/Users/v0d1ch/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "financial_news_scraper_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "financial_news_scraper_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "financial_news_scraper_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "financial_news_scraper_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "financial_news_scraper_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "financial_news_scraper_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
