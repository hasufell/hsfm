{-# OPTIONS_HADDOCK ignore-exports #-}

module IO.File (
    openFile
  , executeFile
  ) where


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
    doesFileExist
  , getPermissions
  , executable
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
