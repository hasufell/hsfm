{-# OPTIONS_HADDOCK ignore-exports #-}

module IO.File where


import Control.Exception
  (
    throw
  )
import Control.Monad
  (
    unless
  , void
  )
import IO.Error
import System.Directory
  (
    doesDirectoryExist
  , doesFileExist
  , executable
  , getPermissions
  , removeDirectory
  , removeFile
  )
import System.FilePath.Posix
  (
    isAbsolute
  )
import System.Process
  (
    spawnProcess
  , ProcessHandle
  )


data FileOperation = Copy
                   | Move
                   | Delete
                   | Open
                   | Execute


-- |Deletes the given file.
--
-- This will throw an exception if the filepath is not absolute
-- or the file does not exist.
--
-- It also throws exceptions from `removeFile`.
-- TODO: threaded, shouldn't block the GUI
deleteFile :: FilePath -> IO ()
deleteFile fp = do
  fileSanityThrow fp
  removeFile fp


-- |Deletes the given directory.
--
-- This will throw an exception if the filepath is not absolute
-- or the directory does not exist.
--
-- It also throws exceptions from `removeDirectory`.
-- TODO: threaded, shouldn't block the GUI
deleteDir :: FilePath -> IO ()
deleteDir fp = do
  dirSanityThrow fp
  removeDirectory fp


-- |Opens a file appropriately by invoking xdg-open.
--
-- This will throw an exception if the filepath is not absolute
-- or the file does not exist.
openFile :: FilePath          -- ^ absolute path to file
         -> IO ProcessHandle
openFile fp = do
  fileSanityThrow fp
  spawnProcess "xdg-open" [fp]


-- |Executes a program with the given arguments.
--
-- This will throw an exception if the filepath is not absolute
-- or the file does not exist. It will also throw an exception
-- if the file is not executable.
executeFile :: FilePath          -- ^ absolute path to program
            -> [String]          -- ^ arguments
            -> IO ProcessHandle
executeFile fp args = do
  fileSanityThrow fp
  p <- getPermissions fp
  unless (executable p) (throw $ FileNotExecutable fp)
  spawnProcess fp args


-- Throws an exception if the filepath is not absolute
-- or the file does not exist.
fileSanityThrow :: FilePath -> IO ()
fileSanityThrow fp = do
  unless (isAbsolute fp) (throw $ PathNotAbsolute fp)
  exists <- doesFileExist fp
  unless exists (throw $ FileDoesNotExist fp)


-- Throws an exception if the filepath is not absolute
-- or the dir does not exist.
dirSanityThrow :: FilePath -> IO ()
dirSanityThrow fp = do
  unless (isAbsolute fp) (throw $ PathNotAbsolute fp)
  exists <- doesDirectoryExist fp
  unless exists (throw $ FileDoesNotExist fp)
