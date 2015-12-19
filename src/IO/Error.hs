{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |Provides error handling.
module IO.Error where


import Control.Exception
import Control.Monad
  (
    unless
  , void
  , when
  )
import Data.List
  (
    isPrefixOf
  )
import Data.Typeable
import IO.Utils
import System.Directory
  (
    doesDirectoryExist
  , doesFileExist
  )
import System.FilePath
  (
    equalFilePath
  , isAbsolute
  , takeFileName
  )


data FmIOException = FileDoesNotExist String
                   | DirDoesNotExist String
                   | PathNotAbsolute String
                   | FileNotExecutable String
                   | SameFile String String
                   | NotAFile String
                   | NotADir String
                   | DestinationInSource String String
                   | DirDoesExist String
  deriving (Show, Typeable)


instance Exception FmIOException


-- Throws an exception if the filepath is not absolute
-- or the file does not exist.
fileSanityThrow :: FilePath -> IO ()
fileSanityThrow fp = throwNotAbsolute fp >> throwFileDoesNotExist fp


-- Throws an exception if the filepath is not absolute
-- or the dir does not exist.
dirSanityThrow :: FilePath -> IO ()
dirSanityThrow fp = throwNotAbsolute fp >> throwDirDoesNotExist fp


throwNotAbsolute :: FilePath -> IO ()
throwNotAbsolute fp = unless (isAbsolute fp) (throw $ PathNotAbsolute fp)


throwDirDoesExist :: FilePath -> IO ()
throwDirDoesExist fp =
  whenM (doesDirectoryExist fp) (throw $ DirDoesExist fp)


throwDirDoesNotExist :: FilePath -> IO ()
throwDirDoesNotExist fp =
  unlessM (doesDirectoryExist fp) (throw $ DirDoesNotExist fp)


throwFileDoesNotExist :: FilePath -> IO ()
throwFileDoesNotExist fp =
  unlessM (doesFileExist fp) (throw $ FileDoesNotExist fp)


throwSameFile :: FilePath -- ^ should be canonicalized
              -> FilePath -- ^ should be canonicalized
              -> IO ()
throwSameFile fp1 fp2 = when (equalFilePath fp1 fp2) (throw $ SameFile fp1 fp2)


throwDestinationInSource :: FilePath -- ^ should be canonicalized
                         -> FilePath -- ^ should be canonicalized
                         -> IO ()
throwDestinationInSource source dest =
  when (source `isPrefixOf` dest) (throw $ DestinationInSource dest source)
