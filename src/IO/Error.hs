{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
fileSanityThrow fp = do
  throwNotAbsolute fp
  throwFileDoesNotExist fp


-- Throws an exception if the filepath is not absolute
-- or the dir does not exist.
dirSanityThrow :: FilePath -> IO ()
dirSanityThrow fp = do
  throwNotAbsolute fp
  throwDirDoesNotExist fp


throwNotAbsolute :: FilePath -> IO ()
throwNotAbsolute fp = unless (isAbsolute fp) (throw $ PathNotAbsolute fp)


throwDirDoesExist :: FilePath -> IO ()
throwDirDoesExist fp = do
  exists <- doesDirectoryExist fp
  when exists (throw $ DirDoesExist fp)


throwDirDoesNotExist :: FilePath -> IO ()
throwDirDoesNotExist fp = do
  exists <- doesDirectoryExist fp
  unless exists (throw $ FileDoesNotExist fp)


throwFileDoesNotExist :: FilePath -> IO ()
throwFileDoesNotExist fp = do
  exists <- doesFileExist fp
  unless exists (throw $ FileDoesNotExist fp)


throwSameFile :: FilePath -- ^ should be canonicalized
              -> FilePath -- ^ should be canonicalized
              -> IO ()
throwSameFile fp1 fp2 = when (equalFilePath fp1 fp2) (throw $ SameFile fp1 fp2)


throwDestinationInSource :: FilePath -- ^ should be canonicalized
                         -> FilePath -- ^ should be canonicalized
                         -> IO ()
throwDestinationInSource source dest =
  when (source `isPrefixOf` dest) (throw $ DestinationInSource dest source)
