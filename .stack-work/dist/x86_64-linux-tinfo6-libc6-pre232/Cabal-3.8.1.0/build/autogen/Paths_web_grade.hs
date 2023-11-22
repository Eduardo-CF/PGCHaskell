{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_web_grade (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/eduardo-cf/Prog.Tipo/PGC/git/PGCHaskell/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/be7c75e8b55ef37246ace8305f6f0f50de7eefe18e6d1e2e67d7394bd4ed12e9/9.4.5/bin"
libdir     = "/home/eduardo-cf/Prog.Tipo/PGC/git/PGCHaskell/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/be7c75e8b55ef37246ace8305f6f0f50de7eefe18e6d1e2e67d7394bd4ed12e9/9.4.5/lib/x86_64-linux-ghc-9.4.5/web-grade-0.1.0.0-DObLewd40VKA9ZHLuY6lFD"
dynlibdir  = "/home/eduardo-cf/Prog.Tipo/PGC/git/PGCHaskell/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/be7c75e8b55ef37246ace8305f6f0f50de7eefe18e6d1e2e67d7394bd4ed12e9/9.4.5/lib/x86_64-linux-ghc-9.4.5"
datadir    = "/home/eduardo-cf/Prog.Tipo/PGC/git/PGCHaskell/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/be7c75e8b55ef37246ace8305f6f0f50de7eefe18e6d1e2e67d7394bd4ed12e9/9.4.5/share/x86_64-linux-ghc-9.4.5/web-grade-0.1.0.0"
libexecdir = "/home/eduardo-cf/Prog.Tipo/PGC/git/PGCHaskell/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/be7c75e8b55ef37246ace8305f6f0f50de7eefe18e6d1e2e67d7394bd4ed12e9/9.4.5/libexec/x86_64-linux-ghc-9.4.5/web-grade-0.1.0.0"
sysconfdir = "/home/eduardo-cf/Prog.Tipo/PGC/git/PGCHaskell/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/be7c75e8b55ef37246ace8305f6f0f50de7eefe18e6d1e2e67d7394bd4ed12e9/9.4.5/etc"

getBinDir     = catchIO (getEnv "web_grade_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "web_grade_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "web_grade_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "web_grade_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "web_grade_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "web_grade_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
